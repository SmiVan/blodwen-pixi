module UFO.Prims

export
%foreign "browser:lambda: (_, a) => console.debug(a)"
prim__consoleDebug : a -> PrimIO ()

export
%foreign "javascript:lambda: () => globalThis"
prim__global : PrimIO AnyPtr

access : {default Nil args: List String} -> {default id act: String -> String} -> {default False offset: Bool} -> String
access {args} {act} {offset} = 
    "javascript:lambda: (" ++ offset_str ++ "p,n" ++ args_str ++ ") =>" ++ act "p[n]"
    where
        offset_str : String
        offset_str = if offset then "_," else ""
        args_str : String
        args_str = foldr (\elem => \acc => "," ++ elem ++ acc) "" args

parameters (parent : AnyPtr, name : String)

    namespace String
        export
        %foreign access
        prim__str_get : PrimIO String
        export
        %foreign access {args=["x"], act=(++"=x")}
        prim__str_set : String -> PrimIO ()

    namespace Number
        export
        %foreign access {act=(\obj => "Number(" ++ obj ++ ")")}
        prim__num_get : PrimIO Double
        export
        %foreign access {args=["x"], act=(++"=x")}
        prim__num_set : Double -> PrimIO ()

        export
        %foreign access {args=["x"], act=(++"+=x")}
        prim__num_add : Double -> PrimIO ()
        export
        %foreign access {args=["x"], act=(++"-=x")}
        prim__num_sub : Double -> PrimIO ()
        export
        %foreign access {args=["x"], act=(++"*=x")}
        prim__num_mul : Double -> PrimIO ()
        export
        %foreign access {args=["x"], act=(++"/=x")}
        prim__num_div : Double -> PrimIO ()
        export
        %foreign access {args=["x"], act=(++"%=x")}
        prim__num_mod : Double -> PrimIO ()
        export
        %foreign access {args=["x"], act=(++"**=x")}
        prim__num_pow : Double -> PrimIO ()

    namespace Array
        export
        %foreign access {act=(\obj => "__prim_js2idris_array(" ++ obj ++ ")")}
        prim__arr_get : PrimIO (List ty)
        export
        %foreign access {args=["arr"], act=(++"=__prim_idris2js_array(arr)"), offset=True}
        prim__arr_set : List ty -> PrimIO ()

    namespace Object
        export
        %foreign access {act=(\obj => obj ++ "===undefined?" ++ obj ++ "=Object():" ++ obj)}
        prim__obj_get : PrimIO AnyPtr -- initialises the object if it is undefined