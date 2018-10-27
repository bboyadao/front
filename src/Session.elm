module Session exposing (Session, fromViewer, navKey)

import Browser.Navigation as Nav

navKey : Session -> Nav.Key
navKey session =
    session.key


-- TYPES


type alias Session =
    { key : Nav.Key }



-- CHANGES


fromViewer : Nav.Key -> Session
fromViewer key =
    { key = key }



