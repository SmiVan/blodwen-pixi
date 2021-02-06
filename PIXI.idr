module PIXI
import ESFFI

data ObservablePoint = MkObsPoint AnyPtr

namespace ObservablePoint

    (.x) : ObservablePoint -> Property.Number
    (.x) (MkObsPoint p) = ESNum p ("x")

    (.y) : ObservablePoint -> Property.Number
    (.y) (MkObsPoint p) = ESNum p ("x")
    

public export
interface DisplayObject a where
    (.internal) : a -> AnyPtr
    -- ++ Container
    -- Graphics
    -- ++ Sprite
    -- ++ Text
    -- BitmapText
    -- TilingSprite
    -- AnimatedSprite
    -- Mesh
    -- NineSlicePlane
    -- SimpleMesh
    -- SimplePlane
    -- SimpleRope

    -- (.pivot) : a -> ObservablePoint
    -- (.position) : a -> ObservablePoint
    -- (.scale) : a -> ObservablePoint
    -- (.skew) : a -> ObservablePoint
    
    (.x) : a -> Property.Number
    (.x) obj = ESNum obj.internal "x"
    (.y) : a -> Property.Number
    (.y) obj = ESNum obj.internal "y"
    (.rotation) : a -> Property.Number
    (.rotation) obj = ESNum obj.internal "rotation"
    (.angle) : a -> Property.Number
    (.angle) obj  = ESNum obj.internal "angle" -- rotation in degrees

    (.pivot) : HasIO io => a -> io ObservablePoint
    (.pivot) a = map MkObsPoint $ primIO $ prim__access a.internal "pivot"
    (.position) : HasIO io => a -> io ObservablePoint
    (.position) a = map MkObsPoint $ primIO $ prim__access a.internal "position"
    (.scale) : HasIO io => a -> io ObservablePoint
    (.scale) a = map MkObsPoint $ primIO $ prim__access a.internal "scale"
    (.skew) : HasIO io => a -> io ObservablePoint
    (.skew) a = map MkObsPoint $ primIO $ prim__access a.internal "skew"

export
data Texture = MkTexture AnyPtr

namespace Texture 
    %foreign "browser:lambda: (str) => PIXI.Texture.from(str)"
    prim__from : String -> PrimIO AnyPtr

    export
    from : HasIO io => String -> io PIXI.Texture
    from str = map MkTexture $ primIO $ prim__from str

export
data Sprite = MkSprite AnyPtr

export
DisplayObject Sprite where
    (.internal) (MkSprite i) = i

namespace Sprite
    %foreign "browser:lambda: (texture) => new PIXI.Sprite(texture)"
    prim__new : AnyPtr -> PrimIO AnyPtr

    export
    new : HasIO io => Texture -> io PIXI.Sprite
    new (MkTexture tex) = map MkSprite $ primIO $ prim__new tex

    %foreign "browser:lambda: (spr, pos) => spr.anchor.set(pos)" 
    prim__anchorset : AnyPtr -> Double -> PrimIO ()
    
    export
    anchorset : HasIO io => Sprite -> Double -> io ()
    anchorset (MkSprite spr) pos = primIO $ prim__anchorset spr pos

    -- %foreign "browser:lambda: (spr, blend) => spr.blendmode = PIXI.BLEND_MODES.ADD" 
    -- prim__blendmodeset : AnyPtr -> String -> PrimIO ()

    -- export
    -- blendmodeset : HasIO io => Sprite -> BlendMode -> io ()
    -- blendmodeset (MkSprite spr) blend = primIO $ prim__blendmodeset spr (show blend)
    

    %foreign "browser:lambda: x => x.blendMode = PIXI.BLEND_MODES.ADD"
    prim__blend_add : AnyPtr -> PrimIO ()

    export
    blendAdd : HasIO io => Sprite -> io ()
    blendAdd (MkSprite s) = primIO $ prim__blend_add s

namespace TextStyle
    public export
    data Align = Left | Center | Right
    Show Align where
        show Left = "left"
        show Center = "center"
        show Right = "right"

    public export
    data Fill = List Double
           -- | CanvasGradient
           -- | CanvasPattern

    public export
    data Gradient = LINEAR_VERTICAL
                  | LINEAR_HORIZONTAL

    %foreign "browser:lambda: () => PIXI.TEXT_GRADIENT"
    prim__gradient_store : PrimIO AnyPtr

    export
    ESEnum Gradient where
       store _ = prim__gradient_store
       name LINEAR_VERTICAL = "LINEAR_VERTICAL"
       name LINEAR_HORIZONTAL = "LINEAR_HORIZONTAL"

    namespace Font
        public export
        data Size = Px Double
                    | Pt Double
                    | Prc Double
                    | Em Double

        export
        Show Size where
            show (Px d) = show d ++ "px"
            show (Pt d) = show d ++ "pt"
            show (Prc d) = show d ++ "%"
            show (Em d) = show d ++ "em"

        -- TODO: Implement checks that d > 0
        
        public export
        data Style = Normal
                    | Italic
                    | Oblique

        export
        Show Style where
            show Normal = "normal"
            show Italic = "italic"
            show Oblique = "oblique"

        public export
        data Variant = VariantNormal
                     | VariantSmallCaps

        public export
        data Weight = WeightNormal
                    | WeightBold
                    | WeightBolder
                    | WeightLighter
                    | Weight100W
                    | Weight200W
                    | Weight300W
                    | Weight400W
                    | Weight500W
                    | Weight600W
                    | Weight700W
                    | Weight800W
                    | Weight900W
        
        Show Weight where
            show WeightNormal = "normal"
            show WeightBold = "bold"
            show WeightBolder = "bolder"
            show WeightLighter = "lighter"
            show Weight100W = "100"
            show Weight200W = "200"
            show Weight300W = "300"
            show Weight400W = "400"
            show Weight500W = "500"
            show Weight600W = "600"
            show Weight700W = "700"
            show Weight800W = "800"
            show Weight900W = "900"

    public export
    data LineJoin = Miter -- sharp corner
                  | Round -- round corner
                  | Bevel -- squared corner

    Show LineJoin where
        show Miter = "miter"
        show Round = "round"
        show Bevel = "bevel"

    public export
    data Whitespace = Normal
                    | Pre
                    | PreLine

    Show Whitespace where
        show Normal = "normal"
        show Pre = "pre"
        show PreLine = "pre-line"

record TextStyle where
    constructor MkTextStyle
    align : Align
    breakWords : Bool
    dropShadow : Bool
    dropShadowAlpha : Double
    dropShadowAngle : Double
    dropShadowBlur : Double
    dropShadowColor : Double
    dropShadowDistance : Double
    fill : Fill
    fillGradientType : Gradient
    fillGradientStops : List Double
    fontFamily : List String
    fontSize : Font.Size
    fontStyle : Font.Style
    fontVariant : Font.Variant
    fontWeight : Font.Weight
    leading : Double
    letterSpacing : Double
    lineHeight : Double
    lineJoin : LineJoin
    miterLimit : Double
    padding : Double
    stroke : Double
    strokeThickness : Double
    trim : Bool
    -- textBaseline : String -- what does this do?
    whiteSpace : Whitespace
    wordWrap : Bool
    wordWrapWidth : Double

export
data Text = MkText AnyPtr

export
DisplayObject Text where
    (.internal) (MkText i) = i
    
namespace Text
    
    %foreign "browser:lambda: (str) => new PIXI.Text(str)"
    prim__new : String -> PrimIO anyPtr

    export
    new : HasIO io => String -> io PIXI.Text
    new str = map MkText $ primIO $ prim__new str


export
data Container = MkContainer AnyPtr

export
DisplayObject Container where
    (.internal) (MkContainer i) = i

namespace Container
    %foreign "browser:lambda: () => new PIXI.Container()"
    prim__newPixiContainer : PrimIO AnyPtr

    export
    new : HasIO io => io PIXI.Container
    new = map MkContainer $ primIO $ prim__newPixiContainer

    %foreign "browser:lambda: (container, displayObject) => container.addChild(displayObject)"
    prim__addChild : (container: AnyPtr) -> (displayObject: AnyPtr) -> PrimIO ()

    export
    addChild : HasIO io => DisplayObject displayObject 
        => Container -> displayObject -> io ()
    addChild (MkContainer container) displayObject =
        primIO $ prim__addChild container displayObject.internal


export
data Application = MkApplication AnyPtr
    
namespace Application     
    %foreign "browser:lambda: 
        (backgroundColor, transparent) => 
        new PIXI.Application({
            backgroundColor: Number(backgroundColor), 
            transparent: transparent,
            resizeTo: window
        })"
    prim__newPixiApplication : (backgroundColor: Int) 
                            -> (transparent : Int)
                            -> PrimIO AnyPtr

    export
    new : HasIO io 
        => {default 0 color : Int} 
        -> {default False transparent : Bool} 
        -> {default False transparent : Bool} 
        -> io PIXI.Application
    new {color} {transparent} = 
        map MkApplication $ primIO $ prim__newPixiApplication color (boolToInt transparent)

    %foreign "browser:lambda: (app) => app.view"
    prim__view : AnyPtr -> PrimIO AnyPtr

    export
    view : HasIO io => (app : Application) -> io AnyPtr
    view (MkApplication anyptr) = primIO $ prim__view anyptr

    %foreign "browser:lambda: (app) => app.stage"
    prim__stage : AnyPtr -> PrimIO AnyPtr

    export 
    stage : HasIO io => (app : Application) -> io Container
    stage (MkApplication anyptr) = map MkContainer $ primIO $ prim__stage anyptr

    %foreign "browser:lambda: (app, callback) => app.ticker.add((delta) => callback(delta)())"
    prim__tickeradd : AnyPtr -> (Double -> PrimIO ()) -> PrimIO () 

    export
    tickerAdd : HasIO io => Application -> (Double -> IO ()) -> io ()
    tickerAdd (MkApplication app) callback = do
        primIO $ prim__tickeradd app (\d => toPrim $ callback d)

    %foreign "browser:lambda: (app) => app.ticker.lastTime"
    prim__tickerlasttime : AnyPtr -> PrimIO Double 

    export
    tickerLastTime : HasIO io => Application -> io Double
    tickerLastTime (MkApplication app) = primIO $ prim__tickerlasttime app

    %foreign "browser:lambda: (app) => app.resize()"
    prim__resize : AnyPtr -> PrimIO ()

    export
    resize : HasIO io => Application -> io ()
    resize (MkApplication app) = primIO $ prim__resize app

    export
    data Screen = MkScreen AnyPtr

    namespace Screen
        export
        width : Screen -> Property.Number
        width (MkScreen s) = ESNum s "width"
        export
        height : Screen -> Property.Number
        height (MkScreen s) = ESNum s "height"

    %foreign "browser:lambda: (app) => app.screen"
    prim__screen : AnyPtr -> PrimIO AnyPtr

    export
    screen : HasIO io => (app : Application) -> io Screen
    screen (MkApplication anyptr) = map MkScreen $ primIO $ prim__screen anyptr

