
:- ['cmpecraft.pro'].

:- init_from_map.


% manhattan_distance(+A, +B, -Distance) :- .
manhattan_distance([H1|T1], [H2|T2], Distance) :-
    Distance is abs(H2-H1) + abs(T2-T1).


% minimum_of_list(+List, -Minimum) :- .
minimum_of_list([Y], Y).

minimum_of_list([H|T], Minimum) :-
    minimum_of_list(T, Y),
    Minimum is min(H,Y).

%first find all objects of specified type.
%List all distances to these objects.
%get the one with min distance.
join_2_lists_([], [], _).
join_2_lists_([H1|T1], [H2|T2], Distance, Key) :-
    (   H2==Distance->  Key is H1);
    join_2_lists_(T1, T2, Distance,Key).

find_objects(State, ObjectType, ObjKey, Object, Distance):-
    State = [AgentDict, ObjectDict, _],
    get_dict(x, AgentDict, X),
    get_dict(y, AgentDict, Y),
    get_dict(ObjKey, ObjectDict, Object),
    get_dict(type, Object, ObjectType).

find_distance(State, ObjectType, ObjKey, Object, Distance):-
    State = [AgentDict, ObjectDict, _],
    get_dict(x, AgentDict, X),
    get_dict(y, AgentDict, Y),
    get_dict(ObjKey, ObjectDict, Object),
    get_dict(type, Object, ObjectType),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    manhattan_distance([X,Y], [Ox, Oy], Distance).


find_nearest_type(State, ObjectType, ObjKey, Object, Distance):-
    State = [AgentDict, ObjectDict, _],

    findall(X, find_distance(State, ObjectType, ObjKey, Object, X), Bag1),    %distances
    findall(X, find_objects(State, ObjectType, X, Object, Distance), Bag2),   %keys

    minimum_of_list(Bag1, Distance),                %find min distance
    join_2_lists_(Bag2, Bag1, Distance, ObjKey),    %find the key of min
    get_dict(ObjKey, ObjectDict, Object).           %get the object



%navigate to(+State, +X, +Y, -ActionList, +DepthLimit)
%find X distance and Y distance to the specified location from the location of agent.
%get actions accordingly.

right(N,[H|T]):-    %recursively add go_right action to the list in the amount of X distance.
    H=go_right,
    N1 is N-1,
    (   N1 > 0 ->  right(N1,T) ;T=[]).

left(N,[H|T]):-     %recursively add go_left action to the list in the amount of X distance.
    H=go_left,
    N1 is N-1,
    (   N1 > 0 ->  left(N1,T) ;T=[]).

up(N,[H|T]):-       %recursively add go_up action to the list in the amount of Y distance.
    H=go_up,
    N1 is N-1,
    (   N1 > 0 ->  up(N1,T) ;T=[]).

down(N,[H|T]):-     %recursively add go_down action to the list in the amount of Y distance.
    H=go_down,
    N1 is N-1,
    (   N1 >0 ->  down(N1,T) ;T=[]).

yCheck(Oy, Y,Ly):-      %find if agent should go down or up
        (Y>Oy ->    Difference is Y-Oy,
                    down(Difference, Ly)
        ;
        Y<Oy ->    Difference is Oy-Y,
                up(Difference, Ly)
            );true.
xCheck(Ox, X, Lx):-     %find if agent should go left or right
        ( X>Ox ->   Difference is X-Ox,
                    right(Difference, Lx)
        ;
        X<Ox  ->   Difference is Ox-X,
                    left(Difference, Lx)
                );true.

navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    
    State = [AgentDict, ObjectDict, _],
    get_dict(x, AgentDict, Ox),
    get_dict(y, AgentDict, Oy),
    manhattan_distance([X,Y], [Ox, Oy], Distance),
    
    (Distance < DepthLimit -> 
        xCheck(Ox,X,Lx),
        yCheck(Oy,Y,Ly)  
                                          
    ),                                    
    append(Lx,Ly,ActionList).
  

% chop_nearest_tree(+State, -ActionList) :- .
%First find nearest tree. Get its location. Navigate to that location and chop it.
chop_nearest_tree(State,ActionList):-
    State = [AgentDict, ObjectDict, _],
    find_nearest_type(State, tree, ObjKey, Object, Distance), 
    get_dict(ObjKey, ObjectDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    Limit is Distance +2,
    navigate_to(State,Ox,Oy,Action1,Limit),
    append(Action1, [left_click_c,left_click_c,left_click_c,left_click_c], ActionList).
    
    

% mine_nearest_stone(+State, -ActionList) :- .
%First find nearest stone. Get its location. Navigate to that location and mine it.
mine_nearest_stone(State, ActionList) :-
    State = [AgentDict, ObjectDict, _],
    find_nearest_type(State, stone, ObjKey, Object, Distance), 
    get_dict(ObjKey, ObjectDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    Limit is Distance +2,
    navigate_to(State,Ox,Oy,Action1,Limit),
    append(Action1, [left_click_c,left_click_c,left_click_c,left_click_c], ActionList).


% gather_nearest_food(+State, -ActionList) :- .
%First find nearest food. Get its location. Navigate to that location and eat it.
gather_nearest_food(State, ActionList) :-
    State = [AgentDict, ObjectDict, _],
    find_nearest_type(State, food, ObjKey, Object, Distance), 
    get_dict(ObjKey, ObjectDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    Limit is Distance +2,
    navigate_to(State,Ox,Oy,Action1,Limit),
    append(Action1, [left_click_c], ActionList).


% collect_requirements(+State, +ItemType, -ActionList) :- .
collect_stick(State, StickList):-
    chop_nearest_tree(State,StickList),
    execute_actions(State,StickList,NewState).

%chop two trees, mine one stone and craft one stick
collect_stone_pickaxe(State, SPList):-
    %tree-tree-stone
    chop_nearest_tree(State,L1),
    execute_actions(State, L1, State2),
    chop_nearest_tree(State2,L2),
    execute_actions(State2, L2,State4),
    mine_nearest_stone(State4,L4),
    execute_actions(State4, L4,State5),
    append(L1,L2,Lx1),
    append(Lx1,L4,L5),
    append(L5,[craft_stick], SPList).

collect_stone_axe(State, SPList):-
    %tree-tree-stone
    chop_nearest_tree(State,L1),
    execute_actions(State, L1, State2),
    chop_nearest_tree(State2,L2),
    execute_actions(State2, L2,State4),
    mine_nearest_stone(State4,L4),
    execute_actions(State4, L4,State5),
    append(L1,L2,Lx1),
    append(Lx1,L4,L5),
    append(L5,[craft_stick], SPList).    
    %append(L1,[],SPList).
    

collect_requirements(State, ItemType, ActionList) :- 
    (ItemType=stick -> collect_stick(State,ActionList)
    ; collect_stone_pickaxe(State,ActionList)
    ).



% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
exists(tree).
exists(stone).
exists(cobblestone).
exists(bedrock).

%finds if the first three X location is empty if not checks for X+1.
%If any X locations are not available for the Y column it checks for the Y+1 column.
x_first(_,_,[],_,_,_,_,_,_).
x_first(X, Y, State, XMin, YMin, XMax, YMax, Width, Height):-
    
    WMax is Width - 2,
    (\+is_ok(X,Y,State),
     y_first(X,Y,State,YMin,YMax,Height),
     X2 is X+1,
     \+is_ok(X2, Y, State),
     y_first(X2,Y,State,YMin,YMax,Height),
     X3 is X+2,
     \+is_ok(X3, Y, State),
     y_first(X3, Y, State,YMin,YMax,Height),
     XMax is X3,
     XMin is X
    );

    rec_x(X, Y, State, XMin, YMin, XMax, YMax, Width, Height);
    Y2 is Y+1,
    rec_x(X, Y2, State, XMin, YMin, XMax, YMax, Width, Height).


rec_x(X, Y, State, XMin, YMin, XMax, YMax, Width, Height):-
    WMax is Width - 3,
    NextX is X+1,
    (NextX < WMax -> x_first(NextX, Y, State, XMin, YMin, XMax, YMax, Width, Height)
    ).

%checks if next three Y location is empty or not.
y_first(X, Y, State,YMin, YMax,Height):-
    HMax is Height -3,
    Y<HMax->
    (\+is_ok(X,Y,State),
    Y2 is Y+1,
    \+is_ok(X, Y2, State),
    Y3 is Y+2,
    \+is_ok(X, Y3, State),
    YMax is Y3,
    YMin is Y
    );
    false.

%calls y_first recursively
rec_y(X, Y, State,YMin, YMax,Height):-
    HMax is Height - 2,
    NextY is Y+1,
    y_first(X, Y, State,YMin, YMax,Height).   


% if sth exists returns true so use as \+is_ok
is_ok(X, Y, State) :-
    State = [_, ObjectDict, _],
    get_dict(_, ObjectDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),    
    X = Ox, Y = Oy,true.

%find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :-
%starts from the beginning of the map
find_castle_location(State, XMin, YMin, XMax, YMax) :-
    width(W),
    height(H),
    x_first(1,1, State, XMin, YMin, XMax, YMax, W, H).



% make_castle(+State, -ActionList) :- .
%Mine stones to acquire the necessary requirements. Execute actions to get new states.
%Find castle location. Go to the center of suitable location and and place cobblestones.
make_castle(State, ActionList) :-    
    mine_nearest_stone(State,L1),
    execute_actions(State, L1,State2),
    mine_nearest_stone(State2,L2),
    execute_actions(State2,L2,State3),
    mine_nearest_stone(State3,L3),
    execute_actions(State3,L3,State4),
    (find_castle_location(State4, XMin, YMin, XMax, YMax) ->
    (XL is XMin + XMax,
    XL2 is XL/2, 
    YL is YMin+YMax,
    YL2 is YL/2,
    navigate_to(State4, XL2, YL2, Action, 100),
    append(L1,L2,A1),
    append(A1,L3,A2),
    append(A2,L4,A3),
    append(A3,Action,A4),    
    append(A4,[place_c,place_e,place_ne,place_n,place_nw,place_w,place_sw,place_s,place_se], ActionList))
    ;false).
