module UFO -- Unified Foreign Object
import Data.List

%foreign "browser:lambda: (_, a) => console.debug(a)"
prim__consoleDebug : a -> PrimIO ()

%foreign "javascript:lambda: () => globalThis"
prim__global : PrimIO AnyPtr
export
global : HasIO io => io AnyPtr
global = primIO $ prim__global

export
debug : HasIO io => a -> io ()
debug = primIO . prim__consoleDebug

export
DEBUG : String -> a -> a
DEBUG msg a = unsafePerformIO (do
    debug msg
    pure a
    )

export
Cast Bool Double where
    cast True = 1.0
    cast False = 0.0

export
Cast Double Bool where
    cast 1.0 = True
    cast _ = False

LabelledTuple : (types : List (String, Type)) -> Type
LabelledTuple Nil = ()
LabelledTuple ((str, ty) :: Nil) = ty
LabelledTuple ((str, ty) :: next) = Pair ty (LabelledTuple next)

mutual
    -- depends on UFO, marshalECMAType
    public export
    data ECMAType : Type where
        ECMAString : ECMAType
        ECMANumber : ECMAType
        ECMABoolean : ECMAType
        ECMAArray : ECMAType -> ECMAType --  A kind of list or array
        ECMAObject : List UFO -> ECMAType -- A kind of tuple or struct
        Or : ECMAType -> ECMAType -> ECMAType -- A kind of either or union
        -- ECMAStrictly : (esty : ECMAType ** List (marshalECMAType esty)) -> ECMAType -- only one of following values

    -- depends on ECMAType
    public export
    data UFO : Type where
        Named : ECMAType -> String -> UFO

    -- depends on ECMAType
    public export
    marshalECMAType : ECMAType -> Type
    marshalECMAType ECMAString = String
    marshalECMAType ECMANumber = Double
    marshalECMAType ECMABoolean = Bool
    marshalECMAType (ECMAArray esty) = List (marshalECMAType esty)
    marshalECMAType (ECMAObject index) = LabelledTuple (map (\(Named esty name) => (name, marshalECMAType esty)) index)
    marshalECMAType (Or esty1 esty2) = Either (marshalECMAType esty1) (marshalECMAType esty2)
    -- marshalECMAType (ECMAStrictly (esty**_)) = marshalECMAType esty

public export
marshal : UFO -> Type
marshal (Named (ty) _) = marshalECMAType ty

access : {default Nil args: List String} -> {default id act: String -> String} -> {default False offset: Bool} -> String
access {args} {act} {offset} = 
    "javascript:lambda: (" ++ offset_str ++ "p,n" ++ args_str ++ ") =>" ++ act "p[n]"
    where
        offset_str : String
        offset_str = if offset then "_," else ""
        args_str : String
        args_str = foldr (\elem => \acc => "," ++ elem ++ acc) "" args

parameters (parent : AnyPtr, name : String)
    %foreign access
    prim__get_str : PrimIO String
    %foreign access {args=["x"], act=(++"=x")}
    prim__put_str : String -> PrimIO ()

    %foreign access
    prim__get_num : PrimIO Double
    %foreign access {args=["x"], act=(++"=x")}
    prim__put_num : Double -> PrimIO ()

    %foreign access {act=(\obj => "__prim_js2idris_array(" ++ obj ++ ")")}
    prim__get_arr : PrimIO (List ty)
    %foreign access {args=["arr"], act=(++"=__prim_idris2js_array(arr)"), offset=True}
    prim__put_arr : List ty -> PrimIO ()

export
get : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> io (marshal ufo)
get parent (ECMAString `Named` name) = primIO $ prim__get_str parent name 
get parent (ECMANumber `Named` name) = primIO $ prim__get_num parent name 
get parent (ECMABoolean `Named` name) = map cast $ primIO $ prim__get_num parent name 
get parent ((ECMAArray esty) `Named` name) = primIO $ prim__get_arr parent name 
get parent (ECMAObject Nil `Named` _) = pure () -- skip pointless operation
get parent (ECMAObject memberList `Named` name) = ?get_obj
get parent ((esty1 `Or` esty2) `Named` name) = ?get_union -- This may require determination of type at javascript level!

export
put : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> (marshal ufo) -> io ()
put parent (ECMAString `Named` name) content = primIO $ prim__put_str parent name content
put parent (ECMANumber `Named` name) content = primIO $ prim__put_num parent name content
put parent (ECMABoolean `Named` name) content = primIO $ prim__put_num parent name (cast content)
put parent ((ECMAArray esty) `Named` name) content = primIO $ prim__put_arr parent name content
put parent (ECMAObject Nil `Named` _) _ = pure () -- skip pointless operation
put parent (ECMAObject memberList `Named` name) content = ?put_obj
put parent ((esty1 `Or` esty2) `Named` name) content = ?put_union

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

namespace SCRATCHPAD
    myStr : UFO
    myStr = ECMAString `Named` "string"

    -- string = "haha"

    myArr : UFO
    myArr = ECMAArray ECMAString `Named` "strings"

    -- strings = ["abra", "cadabra"]

    myNumberObject : UFO
    myNumberObject = ECMAObject [
                        ECMANumber `Named` "one",
                        ECMANumber `Named` "two",
                        ECMANumber `Named` "three"
                        ] `Named` "obj"

    -- obj = {one:1, two:2, three:3}

    obj : (typeOf : (String -> Type) ** List (item : String ** typeOf item))
    obj = (types ** [("one" ** 1), ("two" ** 2), ("three" ** 3), ("truth" ** True)])
        where 
            types : String -> Type
            types "one" = Double
            types "two" = Double
            types "three" = Double
            types "truth" = Bool
            types _ = Double -- not technically correct

    obj2 : List (type : Type ** (String, type))
    obj2 = [(Double ** ("one", 1)), (Double ** ("two", 2)), (Double ** ("three", 3)), (Bool ** ("truth", True))] -- not quite right either

    obj3 : (Double, Double, Double)
    obj3 = (1, 2, 3) -- hmm

    obj4 : LabelledTuple [("one", Double), ("two", Double), ("three", Double)]
    obj4 = (1, 2, 3) -- good enough

    -- Probably the most optimal version for the moment.

    myOptionalThing : UFO
    myOptionalThing = (ECMANumber `Or` (ECMAArray ECMANumber)) `Named` "numOrArr"

    -- numOrArr = 1
    -- numOrArr = [1,2]

    -- myStrictThing : UFO
    -- myStrictThing = ECMAStrictly (ECMAString ** ["please", "thank you"]) `Named` "magicword"

    -- magicword = "please"
    -- magicword = "thank you"