
module Config exposing (root_url,url_login,url_signup,gotoMainsite,url_transcard,final)
import Browser.Navigation exposing(load )

root_url:String
root_url= "http://localhost:8000/api/v1/"

url_login: String
url_login= root_url++"account/login/"

url_signup: String
url_signup = root_url++ "rest-auth/registration/"

url_transcard : String
url_transcard = root_url ++ "trans/"

final : String
final = root_url ++ "final/"

gotoMainsite : Cmd msg
gotoMainsite =
  load "http://localhost:8000/"


