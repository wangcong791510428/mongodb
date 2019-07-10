{application, mongodb,
 [{description, "Client interface to MongoDB, also known as the driver. See www.mongodb.org"},
  {vsn, "0.2.1"},
  {modules, [mongodb_app, mongo, mongo_protocol, mongo_connect, mongo_query, mongo_cursor, mvar, mongodb_tests, mongo_replset, resource_pool, mongo_auth, mongodb_util]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {mongodb_app, []}}
 ]}.
