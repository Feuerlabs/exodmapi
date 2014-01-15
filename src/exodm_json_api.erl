%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Erlang library using JSON transport to access EXODM
%%%
%%% Created : 2013 by Tony Rogvall
%%% @end
%%%-------------------------------------------------------------------
-module(exodm_json_api).

-include_lib("lager/include/log.hrl").

-export([
	 create_account/5,
	 lookup_account/2,
	 list_accounts/3,
	 list_accounts/2,
	 list_account_roles/4,
	 list_account_roles/3,
	 delete_account/2]).
-export([
	 create_yang_module/5,
	 create_yang_module/4,
	 list_yang_modules/5,
	 list_yang_modules/4,
	 list_yang_modules/3,
	 lookup_yang_module/4,
	 lookup_yang_module/3,
	 delete_yang_module/4,
	 delete_yang_module/3,
         list_execution_permission/5,
         list_execution_permission/4]).
-export([
	 create_user/5,
	 list_users/2,
	 delete_user/2,
	 lookup_user/2,
	 add_account_access/4,
	 list_account_users/4,
	 list_account_users/3,
	 list_user_accounts/2,
	 remove_account_access/4]).
-export([
	 create_config_set/5,
	 create_config_set/4,
	 list_config_sets/4,
	 list_config_sets/3,
	 list_config_sets/2,
	 delete_config_set/3,
	 delete_config_set/2,
	 add_config_set_members/4,
	 add_config_set_members/3,
	 remove_config_set_members/4,
	 remove_config_set_members/3]).
-export([
	 create_device_type/4,
	 create_device_type/3,
	 list_device_types/4,
	 list_device_types/3,
	 list_device_types/2,
	 delete_device_type/3,
	 delete_device_type/2]).
-export([
	 create_device_group/4,
	 create_device_group/3,
	 list_device_groups/4,
	 list_device_groups/3,
	 list_device_groups/2,
	 delete_device_group/3,
	 delete_device_group/2,
	 add_device_group_members/4,
	 add_device_group_members/3,
	 remove_device_group_members/4,
	 remove_device_group_members/3]).
-export([
	 create_device/7,
	 create_device/6,
	 create_device/5,
	 delete_devices/3,
	 delete_devices/2,
	 lookup_device/3,
	 lookup_device/2,
	 lookup_device_attributes/4,
	 lookup_device_attributes/3,
	 list_devices/4,
	 list_devices/3,
	 list_devices/2,
	 list_device_group_members/5,
	 list_device_group_members/4,
	 list_device_group_members/3,
	 list_device_type_members/5,
	 list_device_type_members/4,
	 list_device_type_members/3,
	 list_config_set_members/5,
	 list_config_set_members/4,
	 list_config_set_members/3]).

-export([json_request/4]). %% Generic
-export([parse_result/2]).

%% Authorisation
-type option()::
	{url, string()} |
	{user, string()} |
	{password, string()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Admin account requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_account(Name::string(), 
		     Mail::string(), 
		     Passw::string(), 
		     FullName::string(), 
		     Options::list(Option::option())) ->
			    Struct::tuple().

create_account(Name, Mail, Passw, FullName, Options) 
  when is_list(Name), is_list(Mail), is_list(Passw), is_list(FullName), 
       is_list(Options) ->
    json_request("exodm:create-account",
		 [{"name", Name},
                  {"email", Mail},
                  {"password", Passw},
                  {"fullname", FullName}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_accounts(N::integer(),
		   Options::list(Option::option())) ->
			  Struct::tuple().

list_accounts(N, Options) 
  when is_integer(N), N>=0, is_list(Options) ->
    json_request("exodm:list-accounts",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 Options).
%%--------------------------------------------------------------------
-spec list_accounts(N::integer(),
		   Prev::string(),
		   Options::list(Option::option())) ->
			  Struct::tuple().


list_accounts(N, Prev, Options) 
  when is_integer(N), N>=0, is_list(Prev), is_list(Options) ->
    json_request("exodm:list-accounts",
		 [{"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_account(Name::string(), 
		     Options::list(Option::option())) ->
			    Struct::tuple().

delete_account(Name, Options)
  when is_list(Name), is_list(Options) ->
    json_request("exodm:delete-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec lookup_account(Name::string(), 
		     Options::list(Option::option())) ->
			    Struct::tuple().

lookup_account(Name, Options)
  when is_list(Name), is_list(Options) ->
    json_request("exodm:lookup-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_account_roles(Account::string(), 
			 N::integer(),
			 Prev::string(),
			 Options::list(Option::option())) ->
			    Struct::tuple().

list_account_roles(Account, N, Prev, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    json_request("exodm:list-account-roles",
		 [{"account", Account},
                  {"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 admin).

-spec list_account_roles(Account::string(), 
			 N::integer(),
			 Options::list(Option::option())) ->
			    Struct::tuple().

list_account_roles(Account, N, Options)
  when is_list(Account), is_integer(N), N>=0, is_list(Options) ->
    json_request("exodm:list-account-roles",
		 [{"account", Account},
                  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Users requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_user(Name::string(), 
		  Mail::string(), 
		  Passw::string(), 
		  FullName::string(), 
		  Options::list(Option::option())) ->
			 Struct::tuple().

create_user(Name, Mail, Passw, FullName, Options) 
  when is_list(Name), is_list(Mail), is_list(Passw), is_list(FullName), 
       is_list(Options) ->
    json_request("exodm:create-user",
		 [{"uname", Name},
		  {"email", Mail},
		  {"password", Passw},
		  {"fullname", FullName}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_users(N::integer(),
		 Options::list(Option::option())) ->
			    Struct::tuple().

list_users(N, Options) 
  when is_integer(N), N>=0, is_list(Options) ->
    json_request("exodm:list-users",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_user(Name::string(),
		  Options::list(Option::option())) ->
			    Struct::tuple().

delete_user(Name, Options) 
  when is_list(Name), is_list(Options) ->
    json_request("exodm:delete-user",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec lookup_user(Name::string(),
		  Options::list(Option::option())) ->
			    Struct::tuple().

lookup_user(Name, Options) 
  when is_list(Name), is_list(Options) ->
    json_request("exodm:lookup-user",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec add_account_access(Account::string(),
			 Role::string(),
			 User_list::list(User::string()),
			 Options::list(Option::option())) ->
				Struct::tuple().

add_account_access(Account, Role, UserList, Options) 
  when is_list(Account), is_list(Role), is_list(UserList), is_list(Options) ->
    json_request("exodm:add-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_account_users(Account::string(),
			 N::integer(),
			 Options::list(Option::option())) ->
				Struct::tuple().

list_account_users(Account, N, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Options) ->
    json_request("exodm:list-account-users",
		 [{"account", Account},
		  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_account_users(Account::string(),
			 N::integer(),
			 Prev::string(),
			 Options::list(Option::option())) ->
				Struct::tuple().

list_account_users(Account, N, Prev, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    json_request("exodm:list-account-users",
		 [{"account", Account},
		  {"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_user_accounts(Name::string(),
			 Options::list(Option::option())) ->
				Struct::tuple().

list_user_accounts(Name, Options) 
  when is_list(Name), is_list(Options) ->
    json_request("exodm:list-user-accounts",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec remove_account_access(Account::string(),
			    Role::string(),
			    User_list::list(User::string()),
			    Options::list(Option::option())) ->
				Struct::tuple().

remove_account_access(Account, Role, UserList, Options) 
  when is_list(Account), is_list(Role), is_list(UserList), is_list(Options) ->
    json_request("exodm:remove-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Yang modules
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_yang_module(Account::string(),
			 Name::string(), 
			 Repo::string(), 
			 File::string(), 
			 Options::list(Option::option())) ->
			 Struct::tuple().

create_yang_module(Account, Name, Repo, File, Options) 
  when is_list(Account), is_list(Name), is_list(Repo), is_list(File), 
       is_list(Options) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"name", Name},
				 {"repository", Repo},
				 {"yang-module", Bin},
                                 {"account", Account}],
				Options);
	Err = {error, _Reason} ->
	    Err
    end.

-spec create_yang_module(Name::string(), 
			 Repo::string(), 
			 File::string(), 
			 Options::list(Option::option())) ->
			 Struct::tuple().

create_yang_module(Name, Repo, File, Options)  
  when is_list(Name), is_list(Repo), is_list(File), is_list(Options) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"repository", Repo},
				 {"name", Name},
				 {"yang-module", Bin}],
			       Options);
	Err = {error, _Reason} ->
	    Err
    end.
create_yang_module1(Params, Options) when is_list(Params)->
    json_request("exodm:create-yang-module",
		 Params,
		 integer_to_list(random()),
		 Options).


%%--------------------------------------------------------------------
-spec list_yang_modules(Account::string(), 
			Repo::string(), 
			N::integer(),
			Prev::string(), 
			Options::list(Option::option())) ->
			       Struct::tuple().

list_yang_modules(Account, Repo, N, Prev, Options) 
  when is_list(Account), is_list(Repo), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", Prev},
                        {"account", Account}],
		       Options).

-spec list_yang_modules(Account::string(), 
			Repo::string(), 
			N::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple();
		       (Account::string(), 
			N::integer(),
			Prev::string(), 
			Options::list(Option::option())) ->
			       Struct::tuple().

list_yang_modules(Account, Repo, N, Options) 
  when is_list(Account), is_list(Repo), is_integer(N), N>=0, 
       is_list(Options) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", ""},
                        {"account", Account}],
		       Options);
list_yang_modules(Repo, N, Prev, Options) 
  when is_list(Repo), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", Prev}],
		       Options).

-spec list_yang_modules(Repo::string(), 
			N::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple().
list_yang_modules(Repo, N, Options) 
  when is_list(Repo), is_integer(N), N>=0, is_list(Options) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", ""}],
		       Options).

list_yang_modules1(Params, Options) ->
    json_request("exodm:list-yang-modules",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec lookup_yang_module(Account::string(), 
			 Repo::string(), 
			 Name::string(), 
			 Options::list(Option::option())) ->
			       Struct::tuple().

lookup_yang_module(Account, Repo, Name, Options) 
  when is_list(Account), is_list(Repo), is_list(Name), is_list(Options) ->
    lookup_yang_module1([{"repository", Repo},
			{"name", Name},
                        {"account", Account}],
			Options).

-spec lookup_yang_module(Repo::string(), 
			 Name::string(), 
			 Options::list(Option::option())) ->
			       Struct::tuple().

lookup_yang_module(Repo, Name, Options) 
  when is_list(Repo), is_list(Name), is_list(Options)  ->
    lookup_yang_module1([{"repository", Repo},
                         {"name", Name}],
                        Options).

lookup_yang_module1(Params, Options) when is_list(Params) ->
    json_request("exodm:lookup-yang-module",
		 Params,
		 integer_to_list(random()),
		 Options).


%%--------------------------------------------------------------------
-spec delete_yang_module(Account::string(), 
			 Repo::string(), 
			 Name::string(), 
			 Options::list(Option::option())) ->
			       Struct::tuple().

delete_yang_module(Account, Repo, Name, Options) 
  when is_list(Account), is_list(Name), is_list(Repo), is_list(Options) ->
    delete_yang_module1([{"repository", Repo},
                         {"name", Name},
			 {"account", Account}],
			Options).

-spec delete_yang_module(Repo::string(), 
			 Name::string(), 
			 Options::list(Option::option())) ->
			       Struct::tuple().

delete_yang_module(Repo, Name, Options) 
  when is_list(Repo), is_list(Name), is_list(Options) ->
    delete_yang_module1([{"repository", Repo},
			 {"name", Name}],
			Options).

delete_yang_module1(Params, Options) when is_list(Params)->
    json_request("exodm:delete-yang-module",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_execution_permission(Account::string(), 
				Repo::string(), 
				Module::string(),
				Rpc::string(),
				Options::list(Option::option())) ->
				       Struct::tuple().

list_execution_permission(Account, Repo, Module, Rpc, Options) 
  when is_list(Account), is_list(Module), is_list(Rpc), is_list(Options) ->
    list_execution_permission1([{"repository", Repo},
				{"modulename", Module},
				{"rpcname", Rpc},
				{"account", Account}],
			       Options).

-spec list_execution_permission(Repo::string(), 
				Module::string(),
				Rpc::string(),
				Options::list(Option::option())) ->
				       Struct::tuple().

list_execution_permission(Repo, Module, Rpc, Options) 
  when is_list(Module), is_list(Rpc), is_list(Options) ->
    list_execution_permission1([{"repository", Repo},
				{"modulename", Module},
				{"rpcname", Rpc}],
			       Options).

list_execution_permission1(Params, Options) when is_list(Params) ->
    json_request("exodm:list-execution-permission",
		 Params,
		 integer_to_list(random()),
		 Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Config Set
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_config_set(Account::string(),
			Name::string(), 
			File::string(),
			Url::string(),
			Options::list(Option::option())) ->
			       Struct::tuple().

create_config_set(Account, Name, File, Url, Options) 
  when is_list(Account), is_list(Name), is_list(File), is_list(Url), 
       is_list(Options) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url},
                        {"account", Account}],
		       Options).

-spec create_config_set(Name::string(), 
			File::string(),
			Url::string(),
			Options::list(Option::option())) ->
			       Struct::tuple().

create_config_set(Name, File, Url, Options)  
  when is_list(Name), is_list(File), is_list(Url), is_list(Options) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url}],
		       Options).

create_config_set1(Params, Options) ->
    json_request("exodm:create-config-set",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_config_sets(Account::string(),
		       N::integer(),
		       Prev::string(),
		       Options::list(Option::option())) ->
			      Struct::tuple().

list_config_sets(Account, N, Prev, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_config_sets1([{"n", N},
		       {"previous", Prev},
                       {"account", Account}],
		      Options).

-spec list_config_sets(Account::string(),
		       N::integer(),
		       Options::list(Option::option())) ->
			      Struct::tuple();
		      (N::integer(),
		       Prev::string(),
		       Options::list(Option::option())) ->
			      Struct::tuple().

list_config_sets(Account, N, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Options) ->
    list_config_sets1([{"n", N},
		       {"previous", ""},
                       {"account", Account}],
		      Options);
list_config_sets(N, Prev, Options) 
  when is_integer(N), N>=0, is_list(Prev), is_list(Options) ->
    list_config_sets1([{"n", N},
		       {"previous", Prev}],
		      Options).

-spec list_config_sets(N::integer(),
		       Options::list(Option::option())) ->
			      Struct::tuple().
list_config_sets(N, Options) 
  when is_integer(N), N>=0, is_list(Options) ->
    list_config_sets1([{"n", N},
		       {"previous", ""}],
		      Options).

list_config_sets1(Params, Options) ->
    json_request("exodm:list-config-sets",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_config_set(Account::string(),
			Name::integer(),
			Options::list(Option::option())) ->
			      Struct::tuple().

delete_config_set(Account, Name, Options) 
  when is_list(Account), is_list(Name), is_list(Options) ->
    delete_config_set1([{"name", Name},
                        {"account", Account}],
		       Options). 

-spec delete_config_set(Name::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple().
delete_config_set(Name, Options) 
  when is_list(Name), is_list(Options) ->
    delete_config_set1([{"name", Name}],
		       Options).

delete_config_set1(Params, Options) ->
    json_request("exodm:delete-config-set",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec add_config_set_members(Account::string(),
			     Types::list(Type::string()),
			     IDs::list(ID::string()),
			     Options::list(Option::option())) ->
				    Struct::tuple().

add_config_set_members(Account, Types, IDs, Options) 
  when is_list(Account), is_list(Types), is_list(IDs), is_list(Options) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"device-id", {array,IDs}},
                             {"account", Account}],
			    Options).

-spec add_config_set_members(Types::list(Type::string()),
			     IDs::list(ID::string()),
			     Options::list(Option::option())) ->
				    Struct::tuple().

add_config_set_members(Types, IDs, Options)  
  when is_list(Types), is_list(IDs), is_list(Options) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"device-id", {array,IDs}}],
			    Options).

add_config_set_members1(Params, Options) ->
    json_request("exodm:add-config-set-members",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec remove_config_set_members(Account::string(),
				Types::list(Type::string()),
				IDs::list(ID::string()),
				Options::list(Option::option())) ->
				       Struct::tuple().

remove_config_set_members(Account, Types, IDs, Options) 
  when is_list(Account), is_list(Types), is_list(IDs), is_list(Options) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"device-id", {array,IDs}},
                                {"account", Account}],
			       Options).

-spec remove_config_set_members(Types::list(Type::string()),
				IDs::list(ID::string()),
				Options::list(Option::option())) ->
				       Struct::tuple().

remove_config_set_members(Types, IDs, Options)   
  when is_list(Types), is_list(IDs), is_list(Options) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"device-id", {array,IDs}}],
			       Options).

remove_config_set_members1(Params, Options) ->
    json_request("exodm:remove-config-set-members",
		 Params,
		 integer_to_list(random()),
		 Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device type
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_device_type(Account::string(),
			 Type::string(), 
			 Protocol::string(),
			 Options::list(Option::option())) ->
			       Struct::tuple().

create_device_type(Account, Type, Protocol, Options) 
  when is_list(Account), is_list(Type), is_list(Protocol), is_list(Options) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol},
                         {"account", Account}],
			Options).

-spec create_device_type(Type::string(), 
			 Protocol::string(),
			 Options::list(Option::option())) ->
			       Struct::tuple().

create_device_type(Type, Protocol, Options)  
  when is_list(Type), is_list(Protocol), is_list(Options) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol}],
			Options).

create_device_type1(Params, Options) ->
    json_request("exodm:create-device-type",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_device_types(Account::string(),
			N::integer(),
			Prev::string(),
			Options::list(Option::option())) ->
			       Struct::tuple().

list_device_types(Account, N, Prev, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_device_types1([{"n", N},
			{"previous", Prev},
                        {"account", Account}],
		       Options).

-spec list_device_types(N::integer(),
			Prev::string(),
			Options::list(Option::option())) ->
			       Struct::tuple();
		       (Account::string(),
			N::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple().

list_device_types(N, Prev, Options) 
  when is_integer(N), N>=0, is_list(Prev), is_list(Options) ->
    list_device_types1([{"n", N},
			{"previous", Prev}],
		       Options);
list_device_types(Account, N, Options) 
  when is_list(Account), is_integer(N), N>=0 ->
    list_device_types1([{"n", N},
			{"previous", ""},
                        {"account", Account}],
		       Options).

-spec list_device_types(N::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple().

list_device_types(N, Options) 
  when is_integer(N), N>=0, is_list(Options) ->
    list_device_types1([{"n", N},
			{"previous", ""}],
		       Options).

list_device_types1(Params, Options) ->
    json_request("exodm:list-device-types",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_device_type(Account::string(),
			  Type::string(),
			  Options::list(Option::option())) ->
				 Struct::tuple().

delete_device_type(Account, Type, Options) 
  when is_list(Account), is_list(Type), is_list(Options) ->
    delete_device_type1([{"name", Type},
                         {"account", Account}],
			Options).

-spec delete_device_type(Type::string(),
			Options::list(Option::option())) ->
			       Struct::tuple().

delete_device_type(Type, Options)  
  when is_list(Type), is_list(Options) ->
    delete_device_type1([{"name", Type}],
			Options).

delete_device_type1(Params, Options) ->
    json_request("exodm:delete-device-type",
		 Params,
		 integer_to_list(random()),
		 Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device group
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_device_group(Account::string(),
			  Name::string(), 
			  Url::string(),
			  Options::list(Option::option())) ->
				 Struct::tuple().

create_device_group(Account, Name, Url, Options) 
  when is_list(Account), is_list(Name), is_list(Url), is_list(Options) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url},
                          {"account", Account}],
			 Options).

-spec create_device_group(Name::string(), 
			  Url::string(),
			  Options::list(Option::option())) ->
				 Struct::tuple().
create_device_group(Name, Url, Options)  
  when is_list(Name), is_list(Url), is_list(Options) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url}],
			 Options).

create_device_group1(Params, Options) ->
    json_request("exodm:create-device-group",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_device_groups(Account::string(),
			 N::integer(),
			 Prev::string(),
			 Options::list(Option::option())) ->
			       Struct::tuple().

list_device_groups(Account, N, Prev, Options)
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_device_groups1([{"n", N},
			 {"previous", Prev},
                         {"account", Account}],
			Options).

-spec list_device_groups(N::integer(),
			 Prev::string(),
			 Options::list(Option::option())) ->
				Struct::tuple();
		        (Account::string(),
			 N::integer(),
			 Options::list(Option::option())) ->
				Struct::tuple().

list_device_groups(N, Prev, Options)
  when is_integer(N), N>=0, is_list(Prev), is_list(Options) ->
    list_device_groups1([{"n", N},
			 {"previous", Prev}],
			Options);
list_device_groups(Account, N, Options)
  when is_list(Account), is_integer(N), N>=0, is_list(Options) ->
    list_device_groups1([{"n", N},
			 {"previous", ""},
                         {"account", Account}],
			Options).

-spec list_device_groups(N::integer(),
			Options::list(Option::option())) ->
			       Struct::tuple().

list_device_groups(N, Options)
  when is_integer(N), N>=0, is_list(Options) ->
    list_device_groups1([{"n", N},
			 {"previous", ""}],
			Options).

list_device_groups1(Params, Options) ->
    json_request("exodm:list-device-groups",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_device_group(Account::string(),
			  Name::string(), 
			  Options::list(Option::option())) ->
				 Struct::tuple().

delete_device_group(Account, GroupName, Options)
  when is_list(Account), is_list(GroupName), is_list(Options) ->
    delete_device_group1([{"group-id", GroupName},
                          {"account", Account}],
			 Options).

-spec delete_device_group(Name::string(), 
			  Options::list(Option::option())) ->
				 Struct::tuple().

delete_device_group(GroupName, Options)
  when is_list(GroupName), is_list(Options) ->
    delete_device_group1([{"group-id", GroupName}], Options).

delete_device_group1(Params, Options) ->
    json_request("exodm:delete-device-group",
		 %% [{"name", GroupName}], %% When delivered
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec add_device_group_members(Account::string(),
			       Groups::list(Group::string()),
			       IDs::list(ID::string()),
			       Options::list(Option::option())) ->
				    Struct::tuple().

add_device_group_members(Account, Groups, IDs, Options)
  when is_list(Account), is_list(Groups), is_list(IDs), 
       is_list(Options) ->
    add_device_group_members1([{"group-id", {array,Groups}},
			       {"device-id", {array,IDs}},
                               {"account", Account}],
			      Options).

-spec add_device_group_members(Groups::list(Group::string()),
			       IDs::list(ID::string()),
			       Options::list(Option::option())) ->
				    Struct::tuple().

add_device_group_members(Groups, IDs, Options)
  when is_list(Groups), is_list(IDs), is_list(Options) ->
    add_device_group_members1([{"group-id", {array,Groups}},
			       {"device-id", {array,IDs}}],
			      Options).

add_device_group_members1(Params, Options) ->
    json_request("exodm:add-device-group-members",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec remove_device_group_members(Account::string(),
				  Groups::list(Group::string()),
				  IDs::list(ID::string()),
				  Options::list(Option::option())) ->
					 Struct::tuple().

remove_device_group_members(Account, Groups, IDs, Options)
  when is_list(Account), is_list(Groups), is_list(IDs), 
       is_list(Options) ->
    remove_device_group_members1([{"group-id", {array,Groups}}, 
				  {"device-id", {array,IDs}},
                                  {"account", Account}],
				 Options).

-spec remove_device_group_members(Groups::list(Group::string()),
				  IDs::list(ID::string()),
				  Options::list(Option::option())) ->
					 Struct::tuple().

remove_device_group_members(Groups, IDs, Options)   
  when is_list(Groups), is_list(IDs), is_list(Options) ->
    remove_device_group_members1([{"group-id", {array,Groups}}, 
				  {"device-id", {array,IDs}}],
				 Options).

remove_device_group_members1(Params, Options) ->
    json_request("exodm:remove-device-group-members",
		 Params,
		 integer_to_list(random()),
		 Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_device(Account::string(),
		    Id::string(), 
		    Type::string(),
		    ServerKey::string(),
		    DeviceKey::string(),
		    MsIsdn::string(),
		    Options::list(Option::option())) ->
				 Struct::tuple().

create_device(Account, Id, Type, ServerKey, DeviceKey, MsIsdn, Options) 
  when is_list(Account), is_list(Id), is_list(Type), 
       is_list(ServerKey), is_list(DeviceKey), 
       is_list(MsIsdn), is_list(Options) ->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", MsIsdn},
		    {"account", Account}],
		   Options).

-spec create_device(Account::string(),
		    Id::string(), 
		    Type::string(),
		    ServerKey::string(),
		    DeviceKey::string(),
		    Options::list(Option::option())) ->
			   Struct::tuple().

create_device(Account, Id, Type, ServerKey, DeviceKey, Options) 
  when is_list(Account), is_list(Id), is_list(Type), 
       is_list(ServerKey), is_list(DeviceKey), 
       is_list(Options) ->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", "+467331231234"},
		    {"account", Account}],
		   Options).

-spec create_device(Id::string(), 
		    Type::string(),
		    ServerKey::string(),
		    DeviceKey::string(),
		    Options::list(Option::option())) ->
			   Struct::tuple().

create_device(Id, Type, ServerKey, DeviceKey, Options) 
  when is_list(Id), is_list(Type), 
       is_list(ServerKey), is_list(DeviceKey), is_list(Options) ->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", "+467331231234"}],
		   Options).

create_device1(Params, Options) ->
    json_request("exodm:create-device",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec delete_devices(Account::string(),
		     IDs::list(ID::string()), 
		     Options::list(Option::option())) ->
				 Struct::tuple().

delete_devices(Account, IDs, Options) 
  when is_list(Account), is_list(IDs), is_list(Options) ->
    delete_devices1([{"device-id", {array, IDs}},
		     {"account", Account}],
		    Options).

-spec delete_devices(IDs::list(ID::string()), 
		    Options::list(Option::option())) ->
				 Struct::tuple().

delete_devices(IDs, Options) 
  when is_list(IDs), is_list(Options) ->
    delete_devices1([{"device-id", {array, IDs}}],
		    Options).

delete_devices1(Params, Options) ->
    json_request("exodm:delete-devices",
		 Params,
		 integer_to_list(random()),
		 Options).    

%%--------------------------------------------------------------------
-spec lookup_device(Account::string(),
		    ID::string(), 
		    Options::list(Option::option())) ->
			   Struct::tuple().

lookup_device(Account, ID, Options) 
  when is_list(Account), is_list(ID), is_list(Options) ->
    lookup_device1([{"device-id", ID},
		    {"account", Account}],
		   Options).

-spec lookup_device(ID::string(), 
		    Options::list(Option::option())) ->
				 Struct::tuple().

lookup_device(ID, Options) 
  when is_list(ID), is_list(Options) ->
    lookup_device1([{"device-id", ID}],
		    Options).

lookup_device1(Params, Options) ->
    json_request("exodm:lookup-device",
		 Params,
		 integer_to_list(random()),
		 Options).    

%%--------------------------------------------------------------------
-spec lookup_device_attributes(Account::string(),
			       ID::string(), 
			       Attributes::list(string()),
			       Options::list(Option::option())) ->
			   Struct::tuple().

lookup_device_attributes(Account, ID, Attributes, Options) 
  when is_list(Account), is_list(ID), is_list(Attributes), is_list(Options) ->
    lookup_device_attributes1([{"device-id", ID},
			       {"attributes", {array, Attributes}}, 
			       {"account", Account}],
			      Options).

-spec lookup_device_attributes(ID::string(), 
			       Attributes::list(string()), 
			       Options::list(Option::option())) ->
				      Struct::tuple().

lookup_device_attributes(ID, Attributes, Options) 
  when is_list(ID), is_list(Options) ->
    lookup_device_attributes1([{"device-id", ID},
			       {"attributes", {array, Attributes}}],
			      Options).

lookup_device_attributes1(Params, Options) ->
    json_request("exodm:lookup-device-attributes",
		 Params,
		 integer_to_list(random()),
		 Options).    

%%--------------------------------------------------------------------
-spec list_devices(Account::string(),
		   N::integer(),
		   Prev::string(),
		   Options::list(Option::option())) ->
			       Struct::tuple().

list_devices(Account, N, Prev, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_devices1([{"n", N},
		   {"previous", Prev},
                   {"account", Account}],
		  Options).

-spec list_devices(N::integer(),
		   Prev::string(),
		   Options::list(Option::option())) ->
			  Struct::tuple();
		  (Account::string(),
		   N::integer(),
		   Options::list(Option::option())) ->
			       Struct::tuple().

list_devices(N, Prev, Options) 
  when is_integer(N), N>=0, is_list(Prev), is_list(Options)  ->
    list_devices1([{"n", N},
		   {"previous", Prev}],
		  Options);
list_devices(Account, N, Options) 
  when is_list(Account), is_integer(N), N>=0, is_list(Options) ->
    list_devices1([{"n", N},
		   {"previous", ""},
                   {"account", Account}],
		  Options).

-spec list_devices(N::integer(),
		   Options::list(Option::option())) ->
			       Struct::tuple().
list_devices(N, Options) 
  when is_integer(N), N>=0, is_list(Options) ->
    list_devices1([{"n", N},
		   {"previous", ""}],
		  Options).

list_devices1(Params, Options) ->
    json_request("exodm:list-devices",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_device_group_members(Account::string(),
				Group::string(),
				N::integer(),
				Prev::string(),
				Options::list(Option::option())) ->
				       Struct::tuple().

list_device_group_members(Account, Group, N, Prev, Options) 
  when is_list(Account), is_list(Group), is_integer(N), N>=0, 
       is_list(Prev), is_list(Options) ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", Prev},
                                {"account", Account}],
			       Options).

-spec list_device_group_members(Group::string(),
				N::integer(),
				Prev::string(),
				Options::list(Option::option())) ->
				       Struct::tuple();
			       (Account::string(),
				Group::string(),
				N::integer(),
				Options::list(Option::option())) ->
				       Struct::tuple().

list_device_group_members(Group, N, Prev, Options) 
  when is_list(Group), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", Prev}],
			       Options);
list_device_group_members(Account, Group, N, Options) 
  when is_list(Account), is_list(Group), is_integer(N), N>=0, 
       is_list(Options) ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", ""},
                                {"account", Account}],
			       Options).

-spec list_device_group_members(Group::string(),
				N::integer(),
				Options::list(Option::option())) ->
				       Struct::tuple().
list_device_group_members(Group, N, Options) 
  when is_integer(Group), is_integer(N), N>=0, is_list(Options) ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", ""}],
			       Options).

list_device_group_members1(Params, Options) ->
    json_request("exodm:list-device-group-members",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_device_type_members(Account::string(),
			       Name::string(),
			       N::integer(),
			       Prev::string(),
			       Options::list(Option::option())) ->
				      Struct::tuple().

list_device_type_members(Account, Name, N, Prev, Options) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, 
       is_list(Prev), is_list(Options) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", Prev},
                               {"account", Account}],
			      Options).

-spec list_device_type_members(Name::string(),
			       N::integer(),
			       Prev::string(),
			       Options::list(Option::option())) ->
				      Struct::tuple();
			      (Account::string(),
			       Name::string(),
			       N::integer(),
			       Options::list(Option::option())) ->
				      Struct::tuple().

list_device_type_members(Name, N, Prev, Options) 
  when is_list(Name), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""}],
			      Options);
list_device_type_members(Account, Name, N, Options) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, 
       is_list(Options) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""},
                               {"account", Account}],
			      Options).

-spec list_device_type_members(Name::string(),
			       N::integer(),
			       Options::list(Option::option())) ->
				      Struct::tuple().

list_device_type_members(Name, N, Options) 
  when is_list(Name), is_integer(N), N>=0, is_list(Options) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""}],
			      Options).

list_device_type_members1(Params, Options) ->
    json_request("exodm:list-device-type-members",
		 Params,
		 integer_to_list(random()),
		 Options).

%%--------------------------------------------------------------------
-spec list_config_set_members(Account::string(),
			      Name::string(),
			      N::integer(),
			      Prev::string(),
			      Options::list(Option::option())) ->
				      Struct::tuple().

list_config_set_members(Account, Name, N, Prev, Options) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, 
       is_list(Prev), is_list(Options)  ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", Prev},
                              {"account", Account}],
			     Options).

-spec list_config_set_members(Name::string(),
			      N::integer(),
			      Prev::string(),
			      Options::list(Option::option())) ->
				     Struct::tuple();
			     (Account::string(),
			      Name::string(),
			      N::integer(),
			      Options::list(Option::option())) ->
				      Struct::tuple().

list_config_set_members(Name, N, Prev, Options) 
  when is_list(Name), is_integer(N), N>=0, is_list(Prev), 
       is_list(Options) ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", Prev}],
			     Options);
list_config_set_members(Account, Name, N, Options) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, 
       is_list(Options) ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", ""},
                              {"account", Account}],
			     Options).

-spec list_config_set_members(Name::string(),
			      N::integer(),
			      Options::list(Option::option())) ->
				      Struct::tuple().

list_config_set_members(Name, N, Options) 
  when is_list(Name), is_integer(N), N>=0, is_list(Options) ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", ""}],
			     Options).

list_config_set_members1(Params, Options) ->
    json_request("exodm:list-config-set-members",
		 Params,
		 integer_to_list(random()),
		 Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% JSON API
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_request(Request, KeyValueList, TransId, Options) ->
    JsonRequest = json_encode(Request, KeyValueList, TransId),
    case http_post(JsonRequest, Options) of
	{ok, {http_response, _Version, 200, _, _Header}, Data} ->
	    String = binary_to_list(Data),
	    ct:pal("Json request ~p~n,result ~p",
		   [lists:flatten(JsonRequest), String]),
	    {ok, {struct, Values}} = exo_json:decode_string(String),
	    {"jsonrpc","2.0"} = lists:keyfind("jsonrpc",1,Values),
	    {"id",TransId} = lists:keyfind("id",1,Values),
	    lists:keyfind("result",1, Values);
	{ok, {http_response, _Version, 401, Reason, _Header}, _Data} ->
	    {error, Reason};
	{error, _Error} = E ->
	    E
    end.

json_encode(Request, KeyValueList, TransId) ->
    exo_json:encode({struct, [{"jsonrpc", "2.0"},
			   {"method", Request},
			   {"id", TransId},
			   {"params",
			    {struct, KeyValueList}}]}).

http_post(Request, Options) ->
    Url = proplists:get_value(url, Options),
    User  = proplists:get_value(user, Options),
    Pass  = proplists:get_value(password, Options),
    exo_http:wpost(Url,
		   [{'Content-Type', "application/json"}] ++ 
		       exo_http:make_headers(User,Pass),
		   iolist_to_binary(Request)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Parse json result
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_result({error, econnrefused} = E, _Expected) ->
    %% Host down ??
    E;
parse_result(ResultStruct, "ok") ->
    %% Standard
    ?debug("ok: result ~p",[ResultStruct]),
    {"result", {struct,[{"result", "ok"}]}} = ResultStruct,
    ok;
parse_result(ResultStruct, {item, Item}) ->
    ?debug("{item , ~p}: result ~p",[Item, ResultStruct]),
    {"result",{struct,[{"result","ok"},{Item,Value}]}} = ResultStruct,
    Value;
parse_result(ResultStruct, {list, Items}) ->
    %% List result
    ?debug("{list, ~p}: result ~p",[Items, ResultStruct]),
    {"result", {struct,[{Items,{array, List}}]}} = ResultStruct,
    List;
parse_result(ResultStruct, {lookup, Items}) ->
    %% Lookup functions, returns zero or one item ??
    ?debug("{item_list , ~p}: result ~p",[Items, ResultStruct]),
    {"result",{struct,[{"result","ok"},{Items,{array, [{struct, Item}]}}]}} = 
        ResultStruct,
    ?debug("{item_list , ~p}: item ~p",[Items, Item]),
    Item;
parse_result(ResultStruct, {error, Reason}) ->
    %% Expected error
    ?debug("{error, ~p}: result ~p",[Reason, ResultStruct]),
    {"result",{struct,[{"result", Reason}]}} = ResultStruct,
    ok;
parse_result(ResultStruct, resultcode) ->
    ?debug("code: result ~p",[ResultStruct]),
    {"result", {struct,[{"result", Result}| _Tail]}} = ResultStruct,
    Result;
parse_result(_ResultStruct, any) ->
    %% Don't check result
    ?debug("any: result ~p",[_ResultStruct]),
    ok;
parse_result(ResultStruct, _Other) ->
    %% Return everything
    ?debug("~p: result ~p",[_Other,ResultStruct]),
    ResultStruct.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Util
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random() ->
    %% Initialize
    random:seed(now()),
    %% Retreive
    random:uniform(16#1000000).

