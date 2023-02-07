-module(amqp_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  StartFlags = {amqp_controller, start_link, []},

  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

  ChildSpecs = [
    #{
      id => one,
      start => StartFlags,
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [amqp_controller]
    },
    #{
      id => two,
      start => StartFlags,
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [amqp_controller]
    },
    #{
      id => three,
      start => StartFlags,
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [amqp_controller]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
