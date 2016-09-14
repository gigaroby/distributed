-module(file_handler).
-export([handler/1]).


handler(BasePath) ->
	fun({Method, URL, _, _, _}) ->
		case Method of
			get ->
				case handle_file_get(BasePath, URL) of
					{found, Content, MIME} ->
						{200, dict:store("Content-Type", MIME, dict:new()), Content};
					{error, Code, Message} ->
						{Code, dict:new(), Message}
				end;
			_ ->
				{405, dict:new(), "method not allowed"}
		end
	end.


handle_file_get(BasePath, URL) ->
	[DirtyPath | _] = string:tokens(URL, "?"),  % take away query string (if any)
	Path = string:strip(DirtyPath, left, $/),  % strip slash from the beginning (see filename:join as per why)
	FullPath = filename:absname(filename:join(filename:split(BasePath) ++ filename:split(Path))),
	case file:read_file(FullPath) of
		{ok, Binary} -> {found, binary:bin_to_list(Binary), get_mime(FullPath)};
		{error, enoent} -> {error, 404, "file " ++ Path ++ " not found"};
		{error, eaccess} -> {error, 403, "permission denied"};
		{error, eisdir} -> handle_file_get(BasePath, Path ++ "/index.html");
		{error, _} -> {error, 500, "internal server error"}
	end.


get_mime(Path) ->
	[Suffix | _ ] = lists:reverse(string:tokens(Path, ".")),
	case Suffix of
		"html" -> "text/html";
		"css" -> "text/css";
		"txt" -> "text/plain";
		"pdf" -> "application/pdf";
		"zip" -> "application/zip";
		"json" -> "application/json";
		"gif" -> "image/gif";
		"jpg" -> "image/jpg";
		"jpeg" -> "image/jpg";
		"png" -> "image/png";
		_ -> "application/octet-stream"
	end.

