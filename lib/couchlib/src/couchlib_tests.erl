-module(couchlib_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0,
         map_date_title/1,
         map_date_title/2]).

-import(proplists, [get_value/2]).

% Tests TODO:
%
% - Return values for missing docs
% - Conflcts
% - first, next, last, prev

test() ->
    Tests = [
             fun basic_db_test/0,
             fun basic_doc_test/0,
             fun batch_insert_test/0,
             fun select_docs_test/0,
             fun basic_view_test/0,
             fun view_support_test/0,
             fun term_store_test/0,
             fun composite_id_pattern_test/0,
             fun composite_key_test/0,
             fun fruit_price_test/0,
             fun web_source_test/0
             % TODO: fun basic_map_reduce_test/0
    ],
    eunit:test({setup, fun setup/0, Tests}).

setup() ->
    couchlib:start().

basic_db_test() ->

    % Create a new db with a unique name.
    Name = random_dbname(),
    {ok, Db} = couchlib:open(Name),

    % Use info/1 to get info about the db.
    Info = couchlib:info(Db),
    ?assertEqual(Name, get_value(db_name, Info)),

    % We can close the db using close/1 (TODO - what does this do?).
    ?assertEqual(ok, couchlib:close(Db)),

    % Delete using the db name.
    couchlib:delete_db(Name).

basic_doc_test() ->

    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % Docs are created using couchlib_doc:new.
    Doc = couchlib_doc:new(),

    % This is an empty doc, but it's been assigned an ID.
    Id = couchlib_doc:get_id(Doc),
    ?assert(is_binary(Id)),
    ?assert(size(Id) > 0),

    % It has no revisions.
    ?assertEqual(undefined, couchlib_doc:get_rev(Doc)),

    % Store the document using put/2. We get back a revised document.
    {ok, DocR} = couchlib:put(Db, Doc),
    ?assert(couchlib_doc:get_rev(DocR) =/= undefined),

    % The document current has no attributes.
    ?assertEqual([], couchlib_doc:get_attr_names(DocR)),

    % We can modify the revised document.
    Doc2 = couchlib_doc:set_attr(tags, [red, green, blue], DocR),
    {ok, Doc2R} = couchlib:put(Db, Doc2),
    ?assertEqual([tags], couchlib_doc:get_attr_names(Doc2R)),
    ?assertEqual([red, green, blue], couchlib_doc:get_attr(tags, Doc2R)),

    % We can retrieve the document using its ID.
    {ok, Doc3} = couchlib:get(Db, Id),
    ?assertEqual(Id, couchlib_doc:get_id(Doc3)),
    ?assertEqual(couchlib_doc:get_rev(Doc2R), 
                 couchlib_doc:get_rev(Doc3)),

    % To modify a document in the database, we can't just add a
    % new doc with the same ID.
    Doc4 = couchlib_doc:new(Id, [{tags, [dog, cat, bird]}]),
    ?assertThrow(conflict, couchlib:put(Db, Doc4)),

    % We have to modify the last revision and update that.
    Doc5 = couchlib_doc:set_attr(tags, [dog, cat, bird], Doc3),
    {ok, _} = couchlib:put(Db, Doc5),

    % TODO: more on attr manipulation: get_attrs, del_attr, etc.

    % If we retrieve a document that doesn't exist, we get
    % {not_found, missing}:
    ?assertEqual({not_found, missing}, couchlib:get(Db, <<"missing">>)),

    % We can delete the document using delete:
    {ok, {_Start, _RevId}} = couchlib:delete(Db, Id),
    %% TODO: couch jargon leaking through here (I believe) - what makes
    %% the most sense to return for delete? How used?

    % If we retrieve a document that was deleted, we get
    % {not_found, deleted}:
    ?assertEqual({not_found, deleted}, couchlib:get(Db, Id)),

    % Doc IDs must be binary.
    ?assertError(undef, couchdb_doc:new("badid", [])),
    ?assertError(undef, couchdb:get(Db, "badid")),
    ?assertError(undef, couchdb:delete(Db, "badid")),

    couchlib:delete_db(DbName).

batch_insert_test() ->

    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % Use put_many to insert a batch of documents. This can have a 10x increase
    % in throughput at the expense of durability.

    Docs = [couchlib_doc:new() || _ <- lists:seq(1, 10)],
    DocsR = couchlib:put_many(Db, Docs),
    ?assertEqual(10, length(DocsR)),
    ?assertMatch([{ok, _}|_], DocsR),

    couchlib:delete_db(DbName).

select_docs_test() ->

    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % Select something that isn't there.

    ?assertEqual({0, 0, []}, couchlib:select(Db, [])),

    % To select from all docs using ID ranges, use couchlib:select/4. Let's add
    % some docs to select.

    D1 = couchlib_doc:new(<<"1">>, [{name, "doc-1"}]),
    D2 = couchlib_doc:new(<<"2">>, [{name, "doc-2"}]),
    D3 = couchlib_doc:new(<<"3">>, [{name, "doc-3"}]),
    D4 = couchlib_doc:new(<<"4">>, [{name, "doc-4"}]),
    couchlib:put_many(Db, [D1, D2, D3, D4]),

    % We can select every doc in the database ordered by ID as follows:

    {TotalRowCount, Offset, Rows1} = couchlib:select(Db, []),
    ?assertEqual(TotalRowCount, 4),
    ?assertEqual(0, Offset),

    % Rows is a proplist containing id, key, value and optionally doc
    % properties. Let's view the ordered ids for the result.

    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>],
                 [get_value(id, Row) || Row <- Rows1]),

    % In the case when we haven't specified a view (i.e. we're selecting
    % documents from the database directly), the keys are the document ids.

    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>],
                 [get_value(key, Row) || Row <- Rows1]),

    % Result values are the document's revisions.

    ?assertMatch([[{rev, _}], [{rev, _}], [{rev, _}], [{rev, _}]],
                 [get_value(value, Row) || Row <- Rows1]),

    % By default, the documents themselves aren't retured in the results.

    ?assertEqual([undefined, undefined, undefined, undefined],
                 [get_value(doc, Row) || Row <- Rows1]),

    % We can include the documents by specifying the include_docs property.

    {4, 0, Rows2} = couchlib:select(Db, [include_docs]),
    ?assertEqual(["doc-1", "doc-2", "doc-3", "doc-4"],
                 [get_value(name, get_value(doc, Row)) || Row <- Rows2]),

    % To reverse the result (i.e. sort by ID descending), use the reverse
    % property.

    {4, 0, Rows3} = couchlib:select(Db, [reverse]),
    ?assertEqual([<<"4">>, <<"3">>, <<"2">>, <<"1">>],
                 [get_value(id, Row) || Row <- Rows3]),

    % The result can be limited using limit.

    {4, 0, Rows4} = couchlib:select(Db, [{limit, 2}]),
    ?assertEqual([<<"1">>, <<"2">>], [get_value(id, Row) || Row <- Rows4]),

    % Docs can be skipped from the start of the match using skip. The offset in
    % the document list will reflect the skipped docs.

    {4, 2, Rows5} = couchlib:select(Db, [{skip, 2}]),
    ?assertEqual([<<"3">>, <<"4">>], [get_value(id, Row) || Row <- Rows5]),

    % Document can be selected by range using a start and end ID. Either value
    % may be undefined, indicating that the range is unbounded for that
    % side. Let's first find documents that include "2" and "3".

    {4, 1, Rows6} = couchlib:select(Db, <<"2">>, <<"3">>, []),
    ?assertEqual([<<"2">>, <<"3">>], [get_value(id, Row) || Row <- Rows6]),

    {4, 1, Rows6} = couchlib:select(Db, <<"2">>, <<"3">>, []),
    ?assertEqual([<<"2">>, <<"3">>], [get_value(id, Row) || Row <- Rows6]),

    % By default, ID selection is inclusive of the end ID. We can exlucde the
    % end using exclude_end.

    {4, 1, Rows7} = couchlib:select(Db, <<"2">>, <<"3">>, [exclude_end]),
    ?assertEqual([<<"2">>], [get_value(id, Row) || Row <- Rows7]),

    % Here are some unbounded selects.

    {4, 2, Rows8} = couchlib:select(Db, <<"3">>, undefined, []),
    ?assertEqual([<<"3">>, <<"4">>], [get_value(id, Row) || Row <- Rows8]),

    {4, 0, Rows9} = couchlib:select(Db, undefined, <<"2">>, []),
    ?assertEqual([<<"1">>, <<"2">>], [get_value(id, Row) || Row <- Rows9]),

    % Take care when specifying reverse - the start and end IDs must be
    % reversed as well.

    {4, 2, Rows10} = couchlib:select(Db, <<"2">>, <<"1">>, [reverse]),
    ?assertEqual([<<"2">>, <<"1">>], [get_value(id, Row) || Row <- Rows10]),

    {4, 2, Rows11} = couchlib:select(Db, <<"2">>, undefined, [reverse]),
    ?assertEqual([<<"2">>, <<"1">>], [get_value(id, Row) || Row <- Rows11]),

    % TODO - test stale

    couchlib:delete_db(DbName).

basic_view_test() ->

    % These tests require view support.
    couchlib_views:start(),

    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % Views are lazily created indexes on databases. Let's create some
    % documents to index.

    D1 = couchlib_doc:new(
           <<"biking">>,
           [{"_rev", "AE19EBC7654"},
            {"title", "Biking"},
            {"body", "My biggest hobby is mountainbiking. The other day..."},
            {"date", "2009/01/30 18:04:11"}]),
    D2 = couchlib_doc:new(
           <<"bought-a-cat">>,
           [{"_rev", "4A3BBEE711"},
            {"title", "Bought a Cat"},
            {"body", "I went to the the pet store earlier and brought home "
             "a little kitty..."},
            {"date", "2009/02/17 21:13:39"}]),
    D3 = couchlib_doc:new(
           <<"hello-world">>,
           [{"_rev", "43FBA4E7AB"},
            {"title", "Hello World"},
            {"body", "Well hello and welcome to my new blog..."},
            {"date", "2009/01/15 15:52:20"}]),

    couchlib:put_many(Db, [D1, D2, D3]),

    % At a minimum, a view requires a map function that maps documents to zero
    % or more key value pairs. The key/value pairs are indexed by key and can
    % be used to retrieve and sort values quickly.
    %
    % Map functions use "folding" to append key/value pairs to an accumulator.
    %
    % Here's a map function that provides date/title pairs, which will create a
    % view that let's use select and sort documents by date.
    %
    % couchlib_nqs supports map functions in the following forms:
    %
    % - function source code (same as default CouchDB definition)
    % - Binary encoded Erlang fun of arity 2
    % - string {M, F} of a 2-arity function (Doc)
    % - string {M, F, A} of a 3-arity function (Doc, Args)
    %
    % Let's start with a function defined as a string. This follows the pattern
    % used by CouchDB in which map functions are stored as JavaScript.

    MapStr =
        <<"fun(Doc) -> "
          "  [{proplists:get_value(\"date\", Doc),"
          "    proplists:get_value(\"title\", Doc)}]"
          "end.">>,

    % We add a view as a property of a special "design" document. CouchDB uses
    % the "_design/" ID prefix to designate a document as one of these special
    % documents.
    %
    % In addition to the views, the design document must specify a language. In
    % this case, we use "couchlib", which must be registered to use
    % couchlib_nqs (native query server).
    %
    % To be used as views, the binary strings below are required (as opposed to
    % lists).

    DDoc1 = couchlib_doc:new(<<"_design/str">>,
                             [{<<"language">>, <<"couchlib">>},
                              {<<"views">>, [{<<"by_date">>,
                                              [{<<"map">>, MapStr}]}]}]),
    couchlib:put(Db, DDoc1),

    % We can now select using this view.

    R1 = couchlib:select({Db, "str", "by_date"}, []),

    % Here's what we expect from this map:

    Expected = {3,0, [[{id, <<"hello-world">>},
                       {key, "2009/01/15 15:52:20"},
                       {value, "Hello World"}],
                      [{id, <<"biking">>},
                       {key, "2009/01/30 18:04:11"},
                       {value, "Biking"}],
                      [{id, <<"bought-a-cat">>},
                       {key, "2009/02/17 21:13:39"},
                       {value, "Bought a Cat"}]]},
    ?assertEqual(Expected, R1),

    % Note that the document IDs are returned as binary strings even though
    % they were originally inserts as lists. This is how CouchDB stores IDs.
    %
    % In the result, we see that items are returned with the document ID, and
    % the key and value provided by the map function.
    %
    % The rows are also ordered by their key, which is a date in this case.
    %
    % Let's using the same function, but encoded as an Erlang term.

    MapFun = fun(Doc) ->
                     [{get_value("date", Doc), get_value("title", Doc)}]
             end,
    MapFunBin = term_to_binary(MapFun),
    DDoc2 = couchlib_doc:new(<<"_design/fun">>,
                             [{<<"language">>, <<"couchlib">>},
                              {<<"views">>, [{<<"by_date">>,
                                              [{<<"map">>, MapFunBin}]}]}]),
    couchlib:put(Db, DDoc2),

    % Note that the fun uses the imported function get_value/2 from the
    % proplists module. One of the benefits of using an encoded fun is that it
    % provides a persistent closure that is used for indexing.

    % Let's use the view.

    R2 = couchlib:select({Db, "fun", "by_date"}, []),
    ?assertEqual(Expected, R2),

    % Let's now provide a two-tuple of module and function for our map. Note
    % that the term must be terminated with a period.

    MF = <<"{couchlib_tests, map_date_title}.">>,
    DDoc3 = couchlib_doc:new(<<"_design/mf">>,
                             [{<<"language">>, <<"couchlib">>},
                              {<<"views">>, [{<<"by_date">>,
                                              [{<<"map">>, MF}]}]}]),
    couchlib:put(Db, DDoc3),

    % Our exported function map_date_title/2 provides the same mapping as the
    % previous two functions.

    R3 = couchlib:select({Db, "mf", "by_date"}, []),
    ?assertEqual(Expected, R3),

    % Let's round out the examples with the fourth variant: a {M, F, A} tuple
    % representation where A is a list of arguments that are appended to the
    % [Doc] list when calling the function.

    MFA = <<"{couchlib_tests, map_date_title, [myarg]}.">>,
    DDoc4 = couchlib_doc:new(<<"_design/mfa">>,
                             [{<<"language">>, <<"couchlib">>},
                              {<<"views">>, [{<<"by_date">>,
                                              [{<<"map">>, MFA}]}]}]),
    couchlib:put(Db, DDoc4),

    % In this view, we'll be using map_data_title/3 below.

    R4 = couchlib:select({Db, "mfa", "by_date"}, []),
    ?assertEqual(Expected, R4),

    couchlib:delete_db(DbName).

view_support_test() ->

    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % While you can create views as fields in design documents, it is easier to
    % use couchlib's design doc support.
    %
    % E.g. you can define the Map function from basic_view_test/0 (see above)
    % this way.
    Map = fun(Doc) ->
                  [{get_value("date", Doc),
                    get_value("title", Doc)}]
          end,
    DDoc1 = couchlib_design:new("fun", [{view, {"by_date", Map}}]),
    couchlib:put(Db, DDoc1),

    % This is syntactic sugar for the manual process of creatign a design
    % document.
    %
    % Let's add a couple documents to map.

    couchlib:put(Db, couchlib_doc:new(<<"biking">>, 
                                      [{"title", "Biking"},
                                       {"date", "2009/01/30"}])),
    couchlib:put(Db, couchlib_doc:new(<<"hello">>, 
                                      [{"title", "Hello World"},
                                       {"date", "2009/01/15"}])),

    % These are simplified versions of the docs used in basic_view_test. We
    % should get this for our view results.

    Expected = {2, 0, [[{id, <<"hello">>},
                        {key, "2009/01/15"},
                        {value, "Hello World"}],
                       [{id, <<"biking">>},
                        {key, "2009/01/30"},
                        {value, "Biking"}]]},

    R1 = couchlib:select({Db, "fun", "by_date"}, []),
    ?assertEqual(Expected, R1),

    % Using functions directly in a view works well for embedded use (in
    % particular, it provides a persisted closure), but it has a major
    % drawback: it's not previewable in Futon.
    %
    % We can alternatively specify a {M, F} tuple.

    DDoc2 = couchlib_design:new("mf", [{view, {"by_date", {couchlib_tests, 
                                                           map_date_title}}}]),
    couchlib:put(Db, DDoc2),
    R2 = couchlib:select({Db, "mf", "by_date"}, []),
    ?assertEqual(Expected, R2),

    % TODO - multiple views, API for adding, removing, and replacing views

    couchlib:delete_db(DbName).

term_store_test() ->
    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % One of the advantages of using CouchDB as an emebedded Erlang database is
    % that we can store Erlang terms directly without converting back and forth
    % from JavaScript.
    %
    % In this test, we'll illustrate storing terms and using them in views.
    %
    % Let's create a couple documents that have non-standard document bodies
    % (i.e. are not strictly name/value fields).

    couchlib:put(Db, couchlib_term:new(<<"jane">>, {user, "jane", admin})),
    couchlib:put(Db, couchlib_term:new(<<"adam">>, {user, "adam", user})),

    % While these documents can't be viewed in Futon, we can read them.

    {ok, Jane} = couchlib:get(Db, <<"jane">>),
    ?assertEqual({user, "jane", admin}, couchlib_term:term(Jane)),

    {ok, Adam} = couchlib:get(Db, <<"adam">>),
    ?assertEqual({user, "adam", user}, couchlib_term:term(Adam)),

    % Let's create a couple views for these documents: one that indexes by name
    % and another by role. The fun is passed a Doc reference, which is a list
    % of [{<<"_id">>, Id}, {<<"_rev">>, Rev}, Term].

    ByName = fun([_, _, {user, Name, _}]) -> [Name] end,
    ByRole = fun([_, _, {user, _, Role}]) -> [atom_to_list(Role)] end,

    DDoc = couchlib_design:new("funs", [{view, {"by_name", ByName}},
                                        {view, {"by_role", ByRole}}]),
    couchlib:put(Db, DDoc),

    % We can use these to lookup a sorted list of users by either name or role.

    ByNameR = couchlib:select({Db, "funs", "by_name"}, []),
    ?assertEqual({2, 0, [[{id, <<"adam">>}, {key, "adam"}, {value, null}],
                         [{id, <<"jane">>}, {key, "jane"}, {value, null}]]},
                 ByNameR),

    ByRoleR = couchlib:select({Db, "funs", "by_role"}, []),
    ?assertEqual({2, 0, [[{id, <<"jane">>}, {key, "admin"}, {value, null}],
                         [{id, <<"adam">>}, {key, "user"}, {value, null}]]},
                 ByRoleR),

    couchlib:delete_db(DbName).

composite_id_pattern_test() ->
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % CouchDB indexes database docs using a b+tree index on the doc IDs. To
    % group related data using IDs, we can use a "composite ID" pattern.

    % Here's a simple blog post.

    P = couchlib_doc:new(<<"blog-1/post-1">>,
                         [{time, "12:34"},
                          {by, "Frank"},
                          {text, "CouchDB is fun!"}]),
    couchlib:put(Db, P),

    % Here are some comments.

    C1 = couchlib_doc:new(<<"blog-1/post-1/comment-1">>,
                          [{time, "12:35"},
                           {by, "Mary"},
                           {text, "And bouncy!"}]),
    couchlib:put(Db, C1),

    C2 = couchlib_doc:new(<<"blog-1/post-1/comment-2">>,
                          [{time, "12:45"},
                           {by, "Bob"},
                           {text, "Bouncy?"}]),
    couchlib:put(Db, C2),

    % We can retrieve all of the documents associated with a blog post using
    % the "blog-1/" prefix.

    {_, _, Rows} = couchlib:select(Db, <<"blog-1/">>, []),
    ?assertEqual([<<"blog-1/post-1">>,
                  <<"blog-1/post-1/comment-1">>,
                  <<"blog-1/post-1/comment-2">>],
                 [get_value(id, Row) || Row <- Rows]),

    % Document IDs are limited to using list/binary keys. For more flexibility
    % in defining keys from document attributes, use a view (see
    % composite_key_test below).

    couchlib:delete_db(DbName).

composite_key_test() ->
    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % A view can be indexed using any of these types: atom, number, binary, or
    % a list of any of these. We can therefore create composite keys using
    % lists. This is similar to the "composite id pattern" illustrated above
    % but is more flexible.

    % Let's create some blog documents that use randomly assigned IDs and
    % attribute references to related documents. We'll also use a 'type'
    % attribute to distinguish one document type from another.

    B = couchlib_doc:new(<<"899">>,
                         [{type, blog},
                          {name, "My Blog"},
                          {by, "Frank"}]),
    P = couchlib_doc:new(<<"341">>,
                         [{type, post},
                          {blog, "899"},
                          {time, "12:34"},
                          {by, "Frank"},
                          {text, "CouchDB is fun!"}]),
    C1 = couchlib_doc:new(<<"154">>,
                          [{type, comment},
                           {blog, "899"},
                           {post, "341"},
                           {time, "12:35"},
                           {by, "Mary"},
                           {text, "And bouncy!"}]),
    C2 = couchlib_doc:new(<<"022">>,
                          [{type, comment},
                           {blog, "899"},
                           {post, "341"},
                           {time, "12:45"},
                           {by, "Bob"},
                           {text, "Bouncy?"}]),

    couchlib:put_many(Db, [B, P, C1, C2]),

    % Let's also add some other blogs.

    couchlib:put(Db, couchlib_doc:new(<<"102">>, [{type, blog}])),
    couchlib:put(Db, couchlib_doc:new(<<"898">>, [{type, blog}])),
    couchlib:put(Db, couchlib_doc:new(<<"900">>, [{type, blog}])),

    % Getting all of the documents...

    {7, 0, All} = couchlib:select(Db, []),
    ?assertEqual(7, length(All)),

    % Let's create a view that will let us access the hierarchy of blogs,
    % posts, and comments.

    % TODO - Accessing doc IDs using binaries here is fuggly. Would it make
    % more sense to provide them as opaque #doc records and let users access
    % the data using couchlib_doc/couchlib_term (which could be imported for
    % compactness of code).

    % TODO - Not preserving key types (string vs binary) is annoying. Do we
    % have to convert to binary when storing?

    Map =
        fun(Doc) ->
                case get_value(type, Doc) of
                    blog ->
                        [[binary_to_list(get_value(<<"_id">>, Doc))]];
                    post ->
                        [[get_value(blog, Doc),
                          binary_to_list(get_value(<<"_id">>, Doc))]];
                    comment ->
                        [[get_value(blog, Doc),
                          get_value(post, Doc),
                          binary_to_list(get_value(<<"_id">>, Doc))]];
                    _ -> []
                end
        end,
    couchlib:put(Db, couchlib_design:new("blog", [{view, {"list", Map}}])),

    % We can use this to select documents at different levels. To get all
    % of the documents associated with blog ID "899", including posts and
    % comments, we'd use a select like this.

    {_, _, [R1]} = couchlib:select({Db, "blog", "list"}, ["899"], []),
    ["899"] = get_value(key, R1),

    % TODO - It looks like there's a bug here. The result includes {key,
    % ["900"]}, which is an exact match of the end key, despite the
    % exclude_end option.

    {_, _, R2} = couchlib:select({Db, "blog", "list"}, ["899"], ["900"],
                                 [exclude_end]),
    ["900"] = get_value(key, lists:last(R2)),

    % TOTO (cont) - To work around this, we need to tack on a 255 char to the
    % end of the blog ID. We can drop the exclude_end.

    {_, _, BlogDocs} = couchlib:select({Db, "blog", "list"},
                                       ["899"], ["899"++[255]], []),

    % TODO - This is a glaring and annoying inconsistency with the Doc
    % structure being passed into the map funs. Here we're accessing by the
    % atom id - with the map fun, it's <<"_id">>. Granted, they are inherently
    % different structures, but it's still annoying.

    ?assertEqual([<<"899">>, <<"341">>, <<"022">>, <<"154">>],
                 [get_value(id, R) || R <- BlogDocs]),

    % Let's now access the documents associated with a post.

    % TODO - Again, same workaround for range matching described above.

    {_, _, PostDocs} = couchlib:select({Db, "blog", "list"},
                                       ["899", "341"],
                                       ["899", "341"++[255]], []),

    ?assertEqual([<<"341">>, <<"022">>, <<"154">>],
                 [get_value(id, R) || R <- PostDocs]),

    couchlib:delete_db(DbName).

fruit_price_test() ->
    % This is taken from "CouchDB: The Definitive Guide"

    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    D1 = couchlib_doc:new([{"item", "apple"},
                           {"prices", [{"Fresh Mart", 1.59},
                                       {"Price Max", 5.99},
                                       {"Apples Express", 0.79}]}]),
    D2 = couchlib_doc:new([{"item", "orange"},
                           {"prices", [{"Fresh Mart", 1.99},
                                       {"Price Max", 3.19},
                                       {"Citrus Circus", 1.09}]}]),
    D3 = couchlib_doc:new([{"item", "banana"},
                           {"prices", [{"Fresh Mart", 1.99},
                                       {"Price Max", 0.79},
                                       {"Banana Montana", 4.22}]}]),
    M = fun(Doc) ->
                Item = get_value("item", Doc),
                Prices = get_value("prices", Doc),
                lists:map(fun({Store, Price}) ->
                                  {[Item, Price], Store}
                          end, Prices)
        end,
    DD = couchlib_design:new("fruit", [{view, {"price", M}}]),

    couchlib:put_many(Db, [D1, D2, D3, DD]),

    {_, _, Rows} = couchlib:select({Db, "fruit", "price"}, []),

    ?assertEqual([[{key,["apple",0.79]},
                   {value,"Apples Express"}],
                  [{key,["apple",1.59]},
                   {value,"Fresh Mart"}],
                  [{key,["apple",5.99]},
                   {value,"Price Max"}],
                  [{key,["banana",0.79]},
                   {value,"Price Max"}],
                  [{key,["banana",1.99]},
                   {value,"Fresh Mart"}],
                  [{key,["banana",4.22]},
                   {value,"Banana Montana"}],
                  [{key,["orange",1.09]},
                   {value,"Citrus Circus"}],
                  [{key,["orange",1.99]},
                   {value,"Fresh Mart"}],
                  [{key,["orange",3.19]},
                   {value,"Price Max"}]],
                 [proplists:delete(id, Row) || Row <- Rows]),

    couchlib:delete_db(DbName).

web_source_test() ->

    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    % This test illustrates how a database can be used to store "raw" web
    % content and views used to index that content.
    %
    % Here are some "raw" web pages.

    P1 = couchlib_doc:new(<<"doc1">>, 
                          [{src, "/foo/bar1"},
                           {body, "title: bar 1\nscore: 10"},
                           {type, "foo"}]),
    P2 = couchlib_doc:new(<<"doc2">>,
                          [{src, "/foo/bar2"},
                           {body, "title: bar 2\nscore: 4"}]),
    P3 = couchlib_doc:new(<<"doc3">>,
                          [{src, "/baz/bar3"},
                           {body, "title: bar 3"},
                           {type, "baz"}]),
    couchlib:put_many(Db, [P1, P2, P3]),

    % The views will all be a part of an "indexes" design doc.

    TypeF = fun(Doc) ->
                    case proplists:get_value(type, Doc) of
                        undefined -> [];
                        Type -> [Type]
                    end
            end,
    ScoreF = fun(Doc) ->
                     Body = proplists:get_value(body, Doc, ""),
                     case re:run(Body, "score: (\\d+)",
                                 [{capture, all_but_first, list}]) of
                         nomatch -> [];
                         {match, [Score]} -> [list_to_integer(Score)]
                     end
             end,
    TitleF = fun(Doc) ->
                     Body = proplists:get_value(body, Doc, ""),
                     case re:run(Body, "title: (.+)",
                                 [{capture, all_but_first, list}]) of
                         nomatch -> [];
                         {match, [Title]} -> [Title]
                     end
             end,
    Indexes = couchlib_design:new("indexes", 
                                  [{view, {"type", TypeF}},
                                   {view, {"score", ScoreF}},
                                   {view, {"title", TitleF}}]),

    couchlib:put(Db, Indexes),

    % The type index.

    ?assertMatch({2, 0, [[{id, <<"doc3">>}, {key, "baz"}, {value, null}],
                         [{id, <<"doc1">>}, {key, "foo"}, {value, null}]]},
                 couchlib:select({Db, "indexes", "type"}, [])),

    % The score index.

    ?assertMatch({2, 0, [[{id, <<"doc2">>}, {key, 4}, {value, null}],
                         [{id, <<"doc1">>}, {key, 10}, {value, null}]]},
                 couchlib:select({Db, "indexes", "score"}, [])),

    % The title index.

    ?assertMatch({3, 0, [[{id, <<"doc1">>}, {key, "bar 1"}, {value, null}],
                         [{id, <<"doc2">>}, {key, "bar 2"}, {value, null}],
                         [{id, <<"doc3">>}, {key, "bar 3"}, {value, null}]]},
                 couchlib:select({Db, "indexes", "title"}, [])),

    couchlib:delete_db(DbName).

basic_map_reduce_test() ->

    %% TODO: Finish this

    couchlib_views:start(),
    DbName = random_dbname(),
    {ok, Db} = couchlib:open(DbName),

    couchlib:put_many(Db, [couchlib_doc:new([{key, ["a","b","c"]}]),
                           couchlib_doc:new([{key, ["a","b","e"]}]),
                           couchlib_doc:new([{key, ["a","c","m"]}]),
                           couchlib_doc:new([{key, ["b","a","c"]}]),
                           couchlib_doc:new([{key, ["b","a","g"]}])]),
    
    M = fun(Doc) -> [{get_value(key, Doc), 1}] end,
    R = fun(_Keys, Vals, _) -> lists:sum(Vals) end,
    couchlib:put(Db, couchlib_design:new("test", [{view, {"test", M, R}}])),

    {_, _, Rows} = couchlib:select({Db, "test", "test"}, []),
    io:format(user, "############ ~p~n", [Rows]),

    couchlib:delete_db(DbName).

map_date_title(Doc) ->
    [{get_value("date", Doc), get_value("title", Doc)}].

map_date_title(Doc, myarg) ->
    map_date_title(Doc).

random_dbname() ->
    random:seed(erlang:now()),
    lists:concat(["testdb_", random:uniform(1000000)]).
