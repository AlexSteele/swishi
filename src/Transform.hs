module Transform where

import Codec.Picture (DynamicImage)
import qualified Codec.Picture as J
import qualified Vision.Image as F
import Vision.Image.JuicyPixels as Convert
    ( toFridayRGB, toJuicyGrey )

-- FIXME: Lots of JuicyPixels->friday->JuicyPixels conversions

data Transform = Transform {
   name :: String,
   description :: String,
   apply :: DynamicImage -> DynamicImage
}

transforms :: [Transform]
transforms =
  [ blackAndWhiteTransform
  , greyTransform ]

shortSummary :: Transform -> String
shortSummary t = name t ++ " - " ++ description t

blackAndWhiteTransform = Transform {
  name = "bw",
  description = "convert to black and white",
  apply = toBlackAndWhite
}

toBlackAndWhite :: DynamicImage -> DynamicImage
toBlackAndWhite img = result
  where
    rgb = J.convertRGB8 img
    frgb = Convert.toFridayRGB rgb
    fgrey = F.convert frgb :: F.Grey
    pixToBW pix = if pix > 127 then 255 else 0
    bw = F.map pixToBW fgrey
    jbw = Convert.toJuicyGrey bw
    result = J.ImageY8 jbw

greyTransform = Transform {
  name = "grey",
  description = "convert to grey scale",
  apply = toGrey
}

toGrey :: DynamicImage -> DynamicImage
toGrey img = result
  where
    rgb = J.convertRGB8 img
    frgb = Convert.toFridayRGB rgb
    fgrey = F.convert frgb :: F.Grey
    jgrey = Convert.toJuicyGrey fgrey
    result = J.ImageY8 jgrey
