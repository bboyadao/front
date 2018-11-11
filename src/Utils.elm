module Utils exposing (httpErrorString)
import Http exposing(..)
httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text
        Timeout ->
            "Http Timeout"
        NetworkError ->
            "Network Error"
        BadStatus response -> "Invalid Cridential!"  ++ Debug.toString response.body

        BadPayload message response ->
            "Bad Http Payload: "
                ++ Debug.toString message
                ++ " ("
                ++ Debug.toString response.status.code
                ++ ")"



