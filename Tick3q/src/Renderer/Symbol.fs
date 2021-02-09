module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper

// add your own functions as needed


//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int) = 
    {
        Type = BusDecoder (w,a,n)
        X = int pos.X
        Y = int pos.Y
        W = 80
        H = n * 15 + 35

    }
/// demo function - not needed for Tick3 answer
let makeDummyComponent (pos: XYPos): Component =
    { 
        X = int pos.X
        Y = int pos.Y
        W = 0 // dummy
        H = 0 // dummy
        Type = Not // dummy
    }

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//

/// Tick3 answer
let busDecoderView (comp: Component) = 
    match comp.Type with
    |BusDecoder (w,a,n) -> //failwithf "Test"
        let fX = float comp.X
        let fY = float comp.Y
        let fH = float comp.H 
        let fW = float comp.W 


        let scaleFactor=1.0 // to demonstrate svg scaling
        let rotation=0 // to demonstrate svg rotation (in degrees)
        let nList = [0..n-1]
        let dynamicBusDecoderContents = List.map (fun x -> 
            text [ // a demo text svg element
                X 70; 
                Y (30+15*x); 
                Style [
                    TextAnchor "left" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "10px"
                    FontWeight "Bold"
                    Fill "Blue" // demo font color
                ]
            ] [str <| sprintf "%i" (x+a)]) nList
        
        let staticBusDecoderContents = 
                [
                    polygon [ // a demo svg polygon triangle
                        SVGAttr.Points (sprintf "0 0, %f 0, %f %f , 0 %f" fW fW fH fH) 
                        SVGAttr.StrokeWidth "5px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.1
                        SVGAttr.Fill "Blue"] []
                    // line [X1 0.; Y1 0.; X2 0.; Y2 (100.) ; Style [Stroke "Black"]] [
                    //  // child elements of line do not display since children of svg are dom elements
                    //  // and svg will only display on svg canvas, not in dom.
                    //  // hence this is not used
                    // ]
                    text [ // a demo text svg element
                        X 40.; 
                        Y 6.; 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "12px"
                            FontWeight "Bold"
                            Fill "Blue" // demo font color
                        ]
                    ] [str <| sprintf "Bus"]
                    text [ // a demo text svg element
                        X 40.; 
                        Y 16.; 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "12px"
                            FontWeight "Bold"
                            Fill "Blue" // demo font color
                        ]
                    ] [str <| sprintf "Decode"] 
                    // child of text element is text to display
                    text [ // a demo text svg element
                        X 5.; 
                        Y ((fH-20.)/2.); 
                        Style [
                            TextAnchor "left" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Blue" // demo font color
                        ]
                    ] [str <| sprintf "IN [0..%i]" (w-1)] 
                ]
        
        let overallBusDecoderContents = List.append staticBusDecoderContents dynamicBusDecoderContents

        g   [ Style [ 
                // the transform here does rotation, scaling, and translation
                // the rotation and scaling happens with TransformOrigin as fixed point first
                TransformOrigin "0px 50px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
                ]
            
            ]  // g optional attributes in first list
            // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
            // use transform with scale and/or translate and/or rotate to transform group

            
            overallBusDecoderContents
            
    |_-> failwithf "Invalid Component"
    
/// demo function can be deleted
let busDecoderViewDummy (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y 
    // in real code w,a,n would come from the component, but as the busDecoder case is not yet written this
    // is a workaround compatible with the dummy components
    let w,a,n = if fX < 100. then (3,0,8) else (4,3,5) // workaround
    //
    // This code demonstrates svg transformations, not needed for Tick3 but useful.
    // The elmish react syntax here uses CSS style transforms, not SVG attribute transforms. They are different.
    // In addition, svg elements transform under css differently from html.
    // See https://css-tricks.com/transforms-on-svg-elements/ for details if needed.
    //
    let scaleFactor=1.0 // to demonstrate svg scaling
    let rotation=0 // to demonstrate svg rotation (in degrees)
    g   [ Style [ 
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
            TransformOrigin "0px 50px" // so that rotation is around centre of line
            Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
            ]
        
        ]  // g optional attributes in first list
        // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
        // use transform with scale and/or translate and/or rotate to transform group
        [
            polygon [ // a demo svg polygon triangle
                SVGAttr.Points "10 0,10 20, 20 20, 20 0"
                SVGAttr.StrokeWidth "2px"
                SVGAttr.Stroke "Black"
                SVGAttr.FillOpacity 0.1
                SVGAttr.Fill "Blue"] []
            
            line [X1 0.; Y1 0.; X2 0.; Y2 (100.) ; Style [Stroke "Black"]] [
             // child elements of line do not display since children of svg are dom elements
             // and svg will only display on svg canvas, not in dom.
             // hence this is not used
            ] 
            text [ // a demo text svg element
                X 0.; 
                Y 100.; 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "10px"
                    FontWeight "Bold"
                    Fill "Blue" // demo font color
                ]
            ] [str <| sprintf "X=%.0f Y=%.0f" fX fY] // child of text element is text to display
    ]
       


/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =    
    [   // change for Tick3 answer
        makeBusDecoderComponent {X=100.; Y=20.} 3 0 8 // for Tick 3 two components
        makeBusDecoderComponent {X=200.; Y=20.} 4 3 5
        // makeDummyComponent {X=100.; Y=20.} // for Tick 3 two components
        // makeDummyComponent {X=200.; Y=20.} 
    ] 
    |> List.map busDecoderView // change for Tick3 answer
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 200.
                Width 300.   
            ]
        ]   svgEls )


type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w

/// Tick3 answer
let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =
    match comp.Type with
    |BusDecoder(w,a,n) ->
        match w with
        |x when x > 0 ->
            match a with
            |x when x>=0 && ((2.0**(float w))-1.0) >= (float(x)) ->
                    match n with 
                    |x when x>0 && float(x) <= ((2.0**(float w))-(float a)) -> Ok comp
                    |_ -> Error (NIsInvalid, "Component has invalid N")
            |_-> Error (AIsInvalid, "Compoonent has invalid A")
        |_-> Error (WIsInvalid, "Component has invalid W")
    |_ -> failwithf "Invalid Component"
    // failwithf "Not implemented"
    



    


