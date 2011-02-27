%%% -*-erlang-*-
{application, couchlib,
 [{description, "Embedded CouchDB"},
  {vsn, "0.0"},
  {modules, []},
  {registered, []},
  {applications, [kernel,
                  stdlib,
                  sasl,
                  crypto]},
  {mod, {couchlib_app, []}}]}.
