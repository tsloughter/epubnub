%% This is the application resource file (.app file) for the epubnub,
%% application.
{application, epubnub,
  [{description, "Erlang PubNub API"},
   {vsn, "0.0.1"},
   {modules, [epubnub_app,
              epubnub_sup,
              epubnub,

              epn_basic_examples,
              epn_example_client]},
   {registered,[epubnub_sup]},
   {applications, [kernel, stdlib, mochiweb, ibrowse, ssl]},
   {mod, {epubnub_app,[]}},
   {start_phases, []}]}.

