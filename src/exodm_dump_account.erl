%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Function to dump an account from exodm using rpc:s
%%% @end
%%% Created : 2013 by Malotte W Lönne

-module(exodm_dump_account).


-export([run/1,
         run/2]).

%% available api modules
-define(JSON, exodm_json_api).

%% Output Use lager ???
-define(info(S,A), io:format(S ++ "~n",A)).

%% chunk size
-define(N, 10). 

-record(account,
        {aname   :: string()
        }).
        
%%--------------------------------------------------------------------
%% @doc
%%
%% Create a dump of account.
%%
%% @end
%%--------------------------------------------------------------------
-spec run(AName::string()) -> ok.

run(AName) when is_list(AName) -> 
    run(AName, json).

%%--------------------------------------------------------------------
%% @doc
%%
%% Create a dump using json rpc:s.
%%
%% @end
%%--------------------------------------------------------------------
-spec run(AName::string(), Api::atom()) -> ok.

run(AName, json) when is_list(AName)->
    ?info("Dumping account ~s",[AName]),
    %% Mod:configure_api(??),
    run1(?JSON, #account {aname = AName}).



run1(Mod, _Account=#account {aname = AName}) ->
    Roles = all(Mod, list_account_roles, "roles", [AName], "", []),
    ?info("Roles: ~p",[Roles]),
    Yangs = all(Mod, list_yang_modules, "yang-modules", [AName, user], "", []),
    ?info("Yang modules: ~p",[Yangs]),
    Users = all(Mod, list_account_users, "users", [AName], "", []),
    ?info("Users: ~p",[Users]).

    

all(Mod, Fun, Items, Args, Prev, Acc) ->
    case Mod:parse_result(apply(Mod, Fun, Args ++ [?N, Prev]),
                          {list, Items}) of
        [] ->
            Acc;
        List ->
            case length(List) of
                ?N ->
                    %% Might be more to fetch
                    all(Mod, Fun,  Items, 
                        Args, hd(lists:reverse(List)), List ++ Acc);
                _Shorter ->
                    %% All roles fetched
                    List ++ Acc
            end
    end.
