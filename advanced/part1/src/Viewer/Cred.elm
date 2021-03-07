module Viewer.Cred exposing (Cred, addHeader, addHeaderIfAvailable, decoder, encodeToken, getUsername)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Username exposing (Username)



-- TYPES


type Cred =
    Cred Username String

getUsername : Cred -> Username
getUsername (Cred username _ ) = username

getToken : Cred -> String
getToken (Cred _ token) = token


-- SERIALIZATION


decoder : Decoder Cred
decoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Decode.string



-- TRANSFORM


encodeToken : Cred -> Value
encodeToken cred =
    let token = getToken cred
    in
    Encode.string token


addHeader : Cred -> RequestBuilder a -> RequestBuilder a
addHeader cred builder =
    builder
        |> withHeader "authorization" ("Token " ++ getToken cred)


addHeaderIfAvailable : Maybe Cred -> RequestBuilder a -> RequestBuilder a
addHeaderIfAvailable maybeCred builder =
    case maybeCred of
        Just cred ->
            addHeader cred builder

        Nothing ->
            builder
