interface ShapeAsset {
  src: string;
  ariaLabel: string;
}

interface ShapeAssets {
  shapes: ShapeAsset[];
  arrows: ShapeAsset[];
  status: ShapeAsset[];
  computers: ShapeAsset[];
  dateTime: ShapeAsset[];
  files: ShapeAsset[];
  finance: ShapeAsset[];
}

interface GifAsset {
  exampleSrc: string;
}

interface Assets {
  shapes: ShapeAssets;
  gif: GifAsset;
}
