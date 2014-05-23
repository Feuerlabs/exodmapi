%%% -*- coding: latin-1 -*-
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Function to dump an account from exodm using rpc:s
%%% @end
%%% Created : 2013 by Malotte W Lönne

-module(exodm_dump_account).

%% -include_lib("lager/include/log.hrl").
-define(info(F, As), io:format((F)++"\n",(As))).
-define(debug(F, As), io:format((F)++"\n",(As))).

-export([run/1,
         run/2]).

%% available api modules
-define(JSON, exodm_json_api).

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
    Options = exodm_rc:read(),
    run(AName, json, Options).

run(AName, json, Options) ->
    run1(?JSON, #account {aname = AName}, Options).

run1(Mod, _Account=#account {aname = AName}, Options) ->
    application:start(exo),
    Roles = all(Mod, list_account_roles, "roles", [AName], Options),
    ?info("Roles: ~p",[Roles]),
    UsersWithRoles = all(Mod, list_account_users, "users", [AName], Options),
    ?info("Users: ~p",[decode_users(UsersWithRoles, [])]),
    
    Yangs = 
        all(Mod, list_yang_modules, "yang-modules", [AName, "user"], Options),
    ?info("Yang modules: ~p",[Yangs]),
    Sets = all(Mod, list_config_sets, "config-sets", [AName], Options),
    ?info("Config sets: ~p",[Sets]),
    Types = all(Mod, list_device_types, "device-types", [AName], Options),
    ?info("Device types: ~p",[Types]),
    Groups = all(Mod, list_device_groups, "device-groups", [AName], Options),
    ?info("Device groups: ~p",[Groups]),
    Devs = all(Mod, list_devices, "devices", [AName], Options),
    ?info("Devices: ~p",[Devs]),

    %% Store device ids for later checks
    DevIds = [name(Dev) || Dev <-Devs],
    ?info("Device ids: ~p",[DevIds]),

    SetMembers = 
        members(Mod, list_config_set_members, "config-set-members", 
                AName, Sets, Options),
    ?info("Sets with members: ~p",[SetMembers]),
    VerfiedSetMembers = members_exists(SetMembers, DevIds, []),
    ?info("Sets with verified members: ~p",[VerfiedSetMembers]),

    TypeMembers = 
        members(Mod, list_device_type_members, "device-type-members", 
                AName, Types, Options),
    ?info("Types with members: ~p",[TypeMembers]),
    VerfiedTypeMembers = members_exists(TypeMembers, DevIds, []),
    ?info("Types with verified members: ~p",[VerfiedTypeMembers]),

    GroupMembers = 
        members(Mod, list_device_group_members, "device-group-members", 
                AName, Groups, Options),
    ?info("Groups with members: ~p",[GroupMembers]),
    VerfiedGroupMembers = members_exists(GroupMembers, DevIds, []),
    ?info("Groups with verified members: ~p",[VerfiedGroupMembers]),   
    
    %% Write yang sources to file
    YangSrcs = 
        lookup(Mod, lookup_yang_module, {lookup, "yang-modules"}, 
               [AName, "user"], Yangs, Options),
    store_yang_srcs(AName, YangSrcs).
    
all(Mod, Fun, Items, Args, Options) ->
    all(Mod, Fun, Items, Args, "", Options, []).

%% Fetch recursively from table until all are fetched
all(Mod, Fun, Items, Args, Prev, Options, Acc) ->
    case Mod:parse_result(apply(Mod, Fun, Args ++ [?N, Prev]++[Options]),
                          {list, Items}) of
        [] ->
            Acc;
        List ->
            case length(List) of
                ?N ->
                    %% Might be more to fetch
                    all(Mod, Fun,  Items, 
                        Args, hd(lists:reverse(List)), Options, List ++ Acc);
                _Shorter ->
                    %% All roles fetched
                    List ++ Acc
            end
    end.

lookup(Mod, Fun, Result, Args, Ids, Options) ->
    lookup(Mod, Fun, Result, Args, Ids, Options, []).

lookup(_Mod, _Fun, _Item, _Args, [], _Options, Acc) ->
    Acc;
lookup(Mod, Fun, Result, Args, [Id | Rest], Options, Acc) ->
    Res = apply(Mod, Fun, Args ++ [Id]++[Options]),
    Value = Mod:parse_result(Res,Result),
    lookup(Mod, Fun, Result, Args, Rest, Options, [{Id, Value} | Acc]).

%% Fetch all members for a group or type
members(Mod, Fun, Items, AName, Parents, Options) ->
    lists:foldl(fun(PStruct, Acc) ->
                        Parent = name(PStruct),
                        Members = 
                            all(Mod, Fun, Items, [AName, Parent], Options),
                        [{Parent, Members}  | Acc]
                end, [], Parents).

name({struct,[{"name",Name},{"protocol",_P}]}) ->
    %% Type
    Name;
name({struct,[{"gid", Gid}, {"name",_Name},{"notification-url",_Url}]}) ->
    %% Group
    Gid;
name({struct,[{"name", Name}, {"yang", _Yang}, {"notification-url",_Url}]}) ->
    %% Config set
    Name;               
name({struct, DevAttrs}) when is_list(DevAttrs)->
    %% Device
    {"device-id", DevId} = lists:keyfind("device-id", 1, DevAttrs),
    DevId;
name(Name) when is_list(Name)->
    Name.


decode_users([], Acc) ->
    Acc;
decode_users([{struct, [{"name", Name}, {"roles", {array, Roles}}]} | Rest ], Acc) ->
    decode_users(Rest, [{Name, Roles} | Acc]).

members_exists([], _DevIds, Acc) ->
    Acc;
members_exists([{TypeOrGroup, MemberList} | Rest], DevIds, Acc) ->
    members_exists(Rest, DevIds, 
                   [{TypeOrGroup, device_exists(MemberList, DevIds, [])} | Acc]).

device_exists([], _DevIds, Acc) ->
    Acc;
device_exists([DevId | Rest], DevIds, Acc) ->
    case lists:member(DevId, DevIds) of
        true -> 
            device_exists(Rest, DevIds, [DevId | Acc]);
        false ->
            %% Warning ??
            ?info("Device ~p does not exist!!",[DevId]),
            device_exists(Rest, DevIds, Acc)
    end.
    

store_yang_srcs(_AName, []) -> 
    ok;
store_yang_srcs(AName, [{Id, Src} | Rest]) ->
    ?debug("Yang module ~s src ~s .",[Id, Src]),
    case file:write_file(filename:join("/tmp", AName++Id), Src) of
        ok -> ?info("Yang module src ~p stored.",[Id]);
        {error, Reason} -> ?info("Yang module src ~p store failed, reason ~p", [Id, Reason])
    end,
    store_yang_srcs(AName, Rest).
