open Belt

// Re-export RemoteData
module RemoteData = {
  include RemoteData
}

//-----------------------------------------------------------------------------
// Types synonimes
//-----------------------------------------------------------------------------
type json_t = Js.Json.t
type result_t<'a, 'b> = Result.t<'a, 'b>
type decode_t<'a> = result_t<'a, Decco.decodeError>
type decoder_t<'a> = json_t => decode_t<'a>
type encoder_t<'a> = 'a => json_t
type promise_t<'a> = Js.Promise.t<'a>
type url = string
type error = string

//-----------------------------------------------------------------------------
// The HTTP client module type
//-----------------------------------------------------------------------------
module type HTTPClient = {
  let get: url => promise_t<result_t<option<json_t>, error>>
  let post: (url, option<json_t>) => promise_t<result_t<option<json_t>, string>>
  let put: (url, option<json_t>) => promise_t<result_t<option<json_t>, string>>
  let delete: string => promise_t<result_t<unit, string>>
}

//-----------------------------------------------------------------------------
// The HTTP client implementation with bs-fetch
//-----------------------------------------------------------------------------
module BsFetch: HTTPClient = {
  let handleAPICallError = (promise: promise_t<Fetch.Response.t>): promise_t<
    result_t<Fetch.response, string>,
  > => {
    promise
    |> Js.Promise.then_(r => {
      r |> Fetch.Response.ok || r |> Fetch.Response.status == 404
        ? Ok(r)->Js.Promise.resolve
        : Error("API call failed: " ++ Fetch.Response.statusText(r))->Js.Promise.resolve
    })
    |> Js.Promise.catch(err => {
      Js.log2("APICallError", err)
      "Network Error"->Error->Js.Promise.resolve
    })
  }

  let extractJson = (promise: promise_t<result_t<Fetch.Response.t, string>>): promise_t<
    result_t<option<json_t>, string>,
  > => {
    promise |> Js.Promise.then_(result =>
      switch result {
      | Ok(resp) =>
        resp
        |> Fetch.Response.json
        |> Js.Promise.then_(decoded => Ok(decoded->Some)->Js.Promise.resolve)
        |> Js.Promise.catch(_ => Ok(None)->Js.Promise.resolve)
      | Error(e) => Error(e)->Js.Promise.resolve
      }
    )
  }

  let get = (url: string): promise_t<result_t<option<json_t>, string>> =>
    Fetch.fetch(url) |> handleAPICallError |> extractJson

  let postOrPut = (verb, url: string, body: option<json_t>): promise_t<
    result_t<option<json_t>, string>,
  > => {
    let headers = Fetch.HeadersInit.make({
      "Accept": "*",
      "Content-Type": "application/json",
    })
    let req = switch body {
    | None => Fetch.RequestInit.make(~method_=verb, ~headers, ())
    | Some(json) =>
      Fetch.RequestInit.make(
        ~method_=verb,
        ~body=json->Js.Json.stringify->Fetch.BodyInit.make,
        ~headers,
        (),
      )
    }
    Fetch.fetchWithInit(url, req) |> handleAPICallError |> extractJson
  }
  let put = postOrPut(Put)
  let post = postOrPut(Post)
  let delete = (url: string): promise_t<result_t<unit, string>> => {
    let req = Fetch.RequestInit.make(
      ~method_=Delete,
      ~headers=Fetch.HeadersInit.make({"Accept": "*"}),
      (),
    )
    Fetch.fetchWithInit(url, req)
    |> handleAPICallError
    |> Js.Promise.then_(res => res->Result.flatMap(_ => Ok())->Js.Promise.resolve)
  }
}

type state_t<'a> = RemoteData.t<'a, option<'a>, string>
// The action to update the state
type action_t<'a> =
  | NetworkRequestBegin
  | NetworkRequestSuccess('a)
  | NetworkRequestError(string)

type response_t<'a> = result_t<'a, string>
type dispatch_t<'a> = action_t<'a> => unit

type gethook_t<'a> = (state_t<'a>, string => unit, unit => unit)
type posthook_t<'a, 'b> = (state_t<'b>, 'a => unit)

//-----------------------------------------------------------------------------
// Utility functions
//-----------------------------------------------------------------------------
let note = (o: option<'a>, e: 'e): result_t<'a, 'e> =>
  switch o {
  | Some(v) => v->Ok
  | None => e->Error
  }

let deccoErrorToResponse = (r: decode_t<'a>): response_t<'a> =>
  // Convert a DeccoError to a string
  switch r {
  | Ok(v) => v->Ok
  // Todo: better format error
  | Error(e) => e.message->Error
  }

let toLoading = (data: state_t<'a>): state_t<'a> => {
  // Manage transition to the Loading state:
  //   if the data was loaded, make it Loading(some(data))
  //   otherwise, make it Loading(None)
  open RemoteData
  Loading(data |> map(d => Some(d)) |> withDefault(None))
}

let updateRemoteData = (data: state_t<'a>, action: action_t<'a>): state_t<'a> =>
  // Manage transition of the state through action
  switch action {
  | NetworkRequestBegin => data |> toLoading
  | NetworkRequestError(error) => RemoteData.Failure(error)
  | NetworkRequestSuccess(response) => RemoteData.Success(response)
  }

let responseToAction = (response: response_t<'a>): action_t<'a> =>
  // Convert a bs-fetch response to an action
  switch response {
  | Ok(r) => r->NetworkRequestSuccess
  | Error(e) => e->NetworkRequestError
  }

let getWhenNeeded = (state, dispatch, ()) => {
  // An helper for React.useEffect0 to fetch (when needed) the remote data
  switch state {
  | RemoteData.NotAsked => dispatch()
  | _ => ()
  }
  None
}

//-----------------------------------------------------------------------------
// The Hook functor
//-----------------------------------------------------------------------------
module Hook = (CLIENT: HTTPClient) => {
  let get = (url: string, decode: decoder_t<'a>, dispatch: dispatch_t<'a>) => {
    dispatch(NetworkRequestBegin)
    open Js.Promise
    CLIENT.get(url) |> then_(result =>
      switch result {
      | Ok(maybeJson) =>
        maybeJson
        ->note("Need json")
        ->Result.flatMap(json => json->decode->deccoErrorToResponse)
        ->responseToAction
      | Error(e) => e->NetworkRequestError
      }
      ->dispatch
      ->resolve
    )
  }

  let putOrPost = (action, decode: decoder_t<'a>, dispatch: dispatch_t<'a>) => {
    dispatch(NetworkRequestBegin)
    open Js.Promise
    action() |> then_(resp =>
      switch resp {
      | Ok(mjson) =>
        mjson
        ->note("Missing expected JSON from the response!")
        ->Result.flatMap(json => json->decode->deccoErrorToResponse)
        ->responseToAction
        ->dispatch
        ->Ok
      | Error(e) => e->NetworkRequestError->dispatch->Error
      }->resolve
    )
  }

  let post = (url, data) => putOrPost(() => CLIENT.post(url, data))

  // A standalone get hook
  let useGet = (url: string, decoder: decoder_t<'a>): (state_t<'a>, unit => unit) => {
    let (state, setState) = React.useState(() => RemoteData.NotAsked)
    let set_state = s => setState(_prevState => s)
    let dispatch = () => get(url, decoder, r => state->updateRemoteData(r)->set_state)->ignore
    (state, dispatch)
  }

  // A standalone get hook that automatically fetch the remote data
  let useAutoGet = (url: string, decoder: decoder_t<'a>): state_t<'a> => {
    let (state, dispatch) = useGet(url, decoder)

    // Trigger get when not asked
    React.useEffect1(() => {
      dispatch()
      None
    }, [url])
    state
  }

  let usePost = (url: string, encoder: encoder_t<'a>, decoder: decoder_t<'b>): posthook_t<
    'a,
    'b,
  > => {
    let (state, setState) = React.useState(() => RemoteData.NotAsked)
    let set_state = s => setState(_prevState => s)
    let dispatch = data =>
      post(url, data->encoder->Some, decoder, r => state->updateRemoteData(r)->set_state)->ignore
    (state, dispatch)
  }
}
