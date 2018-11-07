module Helper exposing (expectAlmostEqual, expectAlmostEqualErr, expectAlmostEqualV2, expectAlmostEqualV2Err, expectAlmostEqualV3, expectAlmostEqualV3Err, smallFloat, smallNonZeroFloat, v2, v3, v3NonZero)

import Expect
import Fuzz exposing (Fuzzer)
import Math.Vector2 as Ref2
import Math.Vector3 as Ref3 exposing (vec3)
import Vector2 as V2
import Vector3 as V3


smallFloat =
    Fuzz.floatRange -1000 1000


smallNonZeroFloat =
    Fuzz.map3
        (\x y isX ->
            if isX then
                x

            else
                y
        )
        (Fuzz.floatRange 0.001 1000)
        (Fuzz.floatRange -1000 -0.001)
        Fuzz.bool


v2 : Fuzzer V2.Float2
v2 =
    Fuzz.map2 (\x y -> ( x, y )) smallFloat smallFloat


v3 =
    Fuzz.map3 (\x y z -> ( x, y, z )) smallFloat smallFloat smallFloat


v3NonZero =
    Fuzz.map3 (\x y z -> ( x, y, z )) smallNonZeroFloat smallNonZeroFloat smallNonZeroFloat


expectAlmostEqualErr =
    mkAlmostEqFn identity (-) identity


expectAlmostEqual : Float -> Float -> Expect.Expectation
expectAlmostEqual =
    expectAlmostEqualErr 0.1


expectAlmostEqualV2 =
    expectAlmostEqualV2Err 0.1


expectAlmostEqualV3 =
    expectAlmostEqualV3Err 0.1


expectAlmostEqualV2Err =
    mkAlmostEqFn V2.length V2.sub (Ref2.toRecord >> (\i -> ( i.x, i.y )))


expectAlmostEqualV3Err =
    mkAlmostEqFn V3.length V3.sub (Ref3.toRecord >> (\i -> ( i.x, i.y, i.z )))


mkAlmostEqFn len sub toTup e a b =
    let
        err =
            -- elm-linear-algebra uses 32bit floats, that's why we get poor precision
            abs (len (sub a (toTup b)))
    in
    Expect.true "" (err < e)
        |> Expect.onFail ("expected almost equal, failed with error " ++ String.fromFloat err)
