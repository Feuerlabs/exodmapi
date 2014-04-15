%%% -*- coding: latin-1 -*-
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
    lager:info("Dumping account ~s",[AName]),
    %% Mod:configure_api(??),
    run1(?JSON, #account {aname = AName}).


run1(Mod, _Account=#account {aname = AName}) ->
    Roles = all(Mod, list_account_roles, "roles", [AName], "", []),
    lager:info("Roles: ~p",[Roles]),
    UsersWithRoles = all(Mod, list_account_users, "users", [AName], "", []),
    lager:info("Users: ~p",[decode_users(UsersWithRoles, [])]),
    
    Yangs = 
        all(Mod, list_yang_modules, "yang-modules", [AName, "user"], "", []),
    lager:info("Yang modules: ~p",[Yangs]),
    Sets = all(Mod, list_config_sets, "config-sets", [AName], "", []),
    lager:info("Config sets: ~p",[Sets]),
    Types = all(Mod, list_device_types, "device-types", [AName], "", []),
    lager:info("Device types: ~p",[Types]),
    Groups = all(Mod, list_device_groups, "device-groups", [AName], 0, []),
    lager:info("Device groups: ~p",[Groups]),
    Devs = all(Mod, list_devices, "devices", [AName], "", []),
    lager:info("Devices: ~p",[Devs]),

    %% Store device ids for later checks
    DevIds = [name(Dev) || Dev <-Devs],
    lager:info("Device ids: ~p",[DevIds]),

    SetMembers = 
        members(Mod, list_config_set_members, "config-set-members", 
                AName, Sets),
    lager:info("Sets with members: ~p",[SetMembers]),
    VerfiedSetMembers = members_exists(SetMembers, DevIds, []),
    lager:info("Sets with verified members: ~p",[VerfiedSetMembers]),

    TypeMembers = 
        members(Mod, list_device_type_members, "device-type-members", 
                AName, Types),
    lager:info("Types with members: ~p",[TypeMembers]),
    VerfiedTypeMembers = members_exists(TypeMembers, DevIds, []),
    lager:info("Types with verified members: ~p",[VerfiedTypeMembers]),

    GroupMembers = 
        members(Mod, list_device_group_members, "device-group-members", 
                AName, Groups),
    lager:info("Groups with members: ~p",[GroupMembers]),
    VerfiedGroupMembers = members_exists(GroupMembers, DevIds, []),
    lager:info("Groups with verified members: ~p",[VerfiedGroupMembers]),   
    
    %% Write yang sources to file
    YangSrcs = 
        lookup(Mod, lookup_yang_module, {lookup, "yang-modules"}, 
               [AName, "user"], Yangs, []),
    store_yang_srcs(AName, YangSrcs).
    

%% Fetch recursively from table until all are fetched
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

lookup(_Mod, _Fun, _Item, _Args, [], Acc) ->
    Acc;
lookup(Mod, Fun, Result, Args, [Id | Rest], Acc) ->
   lookup(Mod, Fun, Result, Args, Rest, 
          [{Id, Mod:parse_result(apply(Mod, Fun, Args ++ [Id]), Result)} | 
           Acc]).

%% Fetch all members for a group or type
members(Mod, Fun, Items, AName, Parents) ->
    lists:foldl(fun(PStruct, Acc) ->
                        Parent = name(PStruct),
                        Members = 
                            all(Mod, Fun, Items, [AName, Parent], "", []),
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
    {"dev-id", DevId} = lists:keyfind("dev-id", 1, DevAttrs),
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
            lager:info("Device ~p does not exist!!",[DevId]),
            device_exists(Rest, DevIds, Acc)
    end.
    

store_yang_srcs(_AName, []) -> 
    ok;
store_yang_srcs(AName, [{Id, Src} | Rest]) ->
    lager:debug("Yang module ~s src ~s .",[Id, Src]),
    case file:write_file(filename:join("/tmp", AName++Id), Src) of
        ok -> lager:info("Yang module src ~p stored.",[Id]);
        {error, Reason} -> lager:info("Yang module src ~p store failed, reason ~p", [Id, Reason])
    end,
    store_yang_srcs(AName, Rest).
