module Transform where

import Codec.Picture (DynamicImage)
import qualified Codec.Picture as J
import qualified Vision.Image as F
import qualified Vision.Image.Transform as T
import qualified Vision.Image.JuicyPixels as Convert

-- FIXME: Lots of JuicyPixels->friday->JuicyPixels conversions

data Transform = Transform {
   name :: String,
   description :: String,
   apply :: DynamicImage -> DynamicImage
}

transforms :: [Transform]
transforms =
  [ blackAndWhiteTransform
  , greyTransform
  , horizontalFlipTransform
  , verticalFlipTransform ]

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

horizontalFlipTransform = Transform {
  name = "hflip",
  description = "horizontal flip",
  apply = hflip
}

hflip :: DynamicImage -> DynamicImage
hflip = applyRGBTransform T.horizontalFlip

verticalFlipTransform = Transform {
  name = "vflip",
  description = "vertical flip",
  apply = vflip
}

vflip :: DynamicImage -> DynamicImage
vflip = applyRGBTransform T.verticalFlip

applyRGBTransform :: (F.RGB -> F.RGB) -> DynamicImage -> DynamicImage
applyRGBTransform transform img = result
  where
    rgb = J.convertRGB8 img
    fimg = Convert.toFridayRGB rgb
    transformed = transform fimg
    rimg = Convert.toJuicyRGB fimg
    result = J.ImageRGB8 rimg
