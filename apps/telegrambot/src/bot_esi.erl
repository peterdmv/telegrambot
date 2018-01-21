-module(bot_esi).

-export([post/3]).
post(Sid, Env, In) ->
    mod_esi:deliver(Sid, ["Content-Type: text/plain\r\n\r\n"]),
    http_gateway:webhook_update({Env, In}).
