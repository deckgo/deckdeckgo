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

interface PrismAsset {
  definitionSrc: string;
}

interface ChartAssets {
  lineSrc: string;
  lineCompareSrc: string;
  lineNoDatesSrc: string;
  lineMultipleSrc: string;
  pieSrc: string;
  barCompareSrc: string;
}

interface TeamAssets {
  david: string;
  nicolas: string;
}

interface GoogleFont {
  id: string;
  name: string;
  family: string;
}

interface Assets {
  shapes: ShapeAssets;
  gif: GifAsset;
  prism: PrismAsset;
  chart: ChartAssets;
  team: TeamAssets;
  fonts: GoogleFont[];
}
