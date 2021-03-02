module UFO2
import UFO.Prims

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

public export
LabelledTuple : (types : List (String, Type)) -> Type
LabelledTuple Nil = ()
LabelledTuple ((str, ty) :: Nil) = ty
LabelledTuple ((str, ty) :: next) = Pair ty (LabelledTuple next)

public export
interface ForeignType fty where
    marshalGet : fty -> Type
    marshalSet : fty -> Type
    marshalGet = marshalSet
    marshalSet = marshalGet
    get : HasIO io => (parent : AnyPtr) -> (name : String) -> (constructor : fty) -> io (marshalGet constructor)
    set : HasIO io => (parent : AnyPtr) -> (name : String) -> (constructor : fty) -> (marshalSet constructor) -> io ()

public export
interface ForeignType fty => Domain local fty where
    localize : (foreign : fty) -> (marshalGet foreign) -> Maybe local
    foreignize : (foreign : fty) -> local -> (marshalSet foreign)

-- data UFO : Type where
--     Named : ForeignType fty => fty -> String -> UFO

public export
record UFO where
    constructor Named
    {fty : Type}
    foreign : fty
    name : String
    {auto proof : ForeignType fty} -- Similar to to (ForeignType fty =>)

public export
record Sighting where
    constructor At
    ufo : UFO
    location : AnyPtr

namespace Sighting
    marshalGet : (live : Sighting) -> Type
    marshalGet ((foreign `Named` _) `At` _) = marshalGet foreign
    marshalSet : (live : Sighting) -> Type
    marshalSet ((foreign `Named` _) `At` _) = marshalSet foreign


namespace Operation

    public export
    get : HasIO io => (live : Sighting) -> io (marshalGet live) 
    get ((foreign `Named` name) `At` location) = get location name foreign

    infixl 8 ::=, +=, -=, *=, /=, %=, **=

    public export
    (::=) : HasIO io => (live : Sighting) -> (marshalSet live) -> io ()
    (::=) ((foreign `Named` name) `At` location) = set location name foreign

    -- public export
    -- interface ForeignType fty => Add fty where
    --     (+=) : HasIO io => (live : Sighting) -> (marshalSet live) -> io ()


namespace ECMA

    public export
    data StringT = Stringg
    public export
    implementation ForeignType StringT where
        marshalGet Stringg = String
        get parent name Stringg = primIO $ prim__str_get parent name
        set parent name Stringg content = primIO $ prim__str_set parent name content
    -- implementation Livemod StringT where
    --     get =

    public export
    data NumberT = Number
    public export
    implementation ForeignType NumberT where
        marshalGet Number = Double
        get parent name Number = primIO $ prim__num_get parent name
        set parent name Number content = primIO $ prim__num_set parent name content
    public export
    implementation Add NumberT where
        (+=) ((foreign `Named` name) `At` location) content = primIO $ prim__num_add location name content

    public export
    data BooleanT = Boolean
    public export
    implementation ForeignType BooleanT where
        marshalGet Boolean = Bool
        get parent name Boolean = map cast $ primIO $ prim__num_get parent name
        set parent name Boolean content = primIO $ prim__num_set parent name (cast content)

    public export
    data ArrayT : Type where
        Array : ForeignType esty => esty -> ArrayT
    public export
    implementation ForeignType ArrayT where
        marshalGet (Array esty) = List (marshalGet esty)
        get parent name (Array esty) = primIO $ prim__arr_get parent name
        set parent name (Array esty) content = primIO $ prim__arr_set parent name content

    public export
    data ObjectT : Type where
        Object : List UFO -> ObjectT
    public export
    implementation ForeignType ObjectT where
        marshalGet (Object index) = LabelledTuple (map (\(Named esty name) => (name, marshalGet esty)) index)
        get parent _ (Object Nil) = pure () -- skip pointless operation
        get parent name (Object memberList) = do
            obj_ptr <- primIO $ prim__obj_get parent name
            case memberList of
                Nil => pure () -- unreachable
                ((esty `Named` name) :: Nil) => do -- last member
                    member <- get obj_ptr name esty
                    pure member
                ((esty `Named` name) :: next) => do -- many members
                    member <- get obj_ptr name esty
                    members <- get parent name (Object next)
                    pure $ believe_me (MkPair member members) -- TODO: not sure how to solve the complicated type mismatch here, take a look at this later
        -- set parent _ (Object Nil) content = pure () -- skip pointless operation


    public export
    data UnionT : Type where
        Or : ForeignType esty1 => ForeignType esty2 => esty1 -> esty2 -> UnionT
    public export
    implementation ForeignType UnionT where
        marshalGet (Or esty1 esty2) = Either (marshalGet esty1) (marshalGet esty2)
        -- get parent ((esty1 `Or` esty2) `Named` name) = DEBUG "?get_union" (believe_me ()) -- This may require determination of type at javascript level!


    public export
    data RestrictedT : Type where
        In : (foreign : fty) -> (local : Type) -> Domain local fty => RestrictedT
    public export                                                               
    implementation ForeignType RestrictedT where
        marshalGet (In esty ty) = Maybe ty -- now safe from crashes!
        marshalSet (In esty ty) = ty
        get parent name (esty `In` ty) = do 
            raw <- get parent name esty
            pure (localize esty raw)
        set parent name (esty `In` ty) content = set parent name esty (foreignize esty content)