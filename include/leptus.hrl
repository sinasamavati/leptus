-type cookie_option() :: {max_age, non_neg_integer()}
                       | {domain, binary()} | {path, binary()}
                       | {secure, boolean()} | {http_only, boolean()}.
-type cookie_opts() :: [cookie_option()].
-export_type([cookie_opts/0]).

-type content_decode_fun() :: fun((binary())
                                  -> {ok, binary()}
                                         | {error, atom()}).
-type transfer_decode_fun() :: fun((binary(), any())
                                   -> {ok, binary(), binary(), any()}
                                          | more | {more, non_neg_integer(), binary(), any()}
                                          | {done, non_neg_integer(), binary()}
                                          | {done, binary(), non_neg_integer(), binary()}
                                          | {error, atom()}).

-type resp_body_fun() :: fun((any(), module()) -> ok).
-type send_chunk_fun() :: fun((iodata()) -> ok | {error, atom()}).
-type resp_chunked_fun() :: fun((send_chunk_fun()) -> ok).

-record(http_req, {
          %% Transport.
          socket = undefined :: any(),
          transport = undefined :: undefined | module(),
          connection = keepalive :: keepalive | close,

          %% Request.
          pid = undefined :: pid(),
          method = <<"GET">> :: binary(),
          version = 'HTTP/1.1' :: cowboy:http_version(),
          peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
          host = undefined :: undefined | binary(),
          host_info = undefined :: undefined | cowboy_router:tokens(),
          port = undefined :: undefined | inet:port_number(),
          path = undefined :: binary(),
          path_info = undefined :: undefined | cowboy_router:tokens(),
          qs = undefined :: binary(),
          qs_vals = undefined :: undefined | list({binary(), binary() | true}),
          bindings = undefined :: undefined | cowboy_router:bindings(),
          headers = [] :: cowboy:http_headers(),
          p_headers = [] :: [any()], %% @todo Improve those specs.
          cookies = undefined :: undefined | [{binary(), binary()}],
          meta = [] :: [{atom(), any()}],

          %% Request body.
          body_state = waiting :: waiting | done | {stream, non_neg_integer(),
                                                    transfer_decode_fun(), any(), content_decode_fun()},
          multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | chunks | done,
	resp_headers = [] :: cowboy:http_headers(),
	resp_body = <<>> :: iodata() | resp_body_fun()
		| {non_neg_integer(), resp_body_fun()}
		| {chunked, resp_chunked_fun()},

	%% Functions.
	onresponse = undefined :: undefined | already_called
		| cowboy:onresponse_fun()
}).

-opaque req() :: #http_req{}.
-export_type([req/0]).
