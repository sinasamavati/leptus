%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-record(http_req, {
          %% Transport.
          socket,
          transport,
          connection,

          %% Request.
          pid,
          method,
          version,
          peer,
          host,
          host_info,
          port,
          path,
          path_info,
          qs,
          qs_vals,
          bindings,
          headers,
          p_headers,
          cookies,
          meta,

          %% Request body.
          body_state,
          multipart,
          buffer,

          %% Response.
          resp_compress,
          resp_state,
          resp_headers,
          resp_body,

          %% Functions.
          onresponse
         }).
