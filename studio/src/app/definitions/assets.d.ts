interface ImgAsset {
  src: string;
  ariaLabel: string;
}

interface ShapeAssets {
  shapes: ImgAsset[];
  arrows: ImgAsset[];
  status: ImgAsset[];
  computers: ImgAsset[];
  dateTime: ImgAsset[];
  files: ImgAsset[];
  finance: ImgAsset[];
}

interface GifAsset {
  exampleSrc: string;
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

interface DeckDeckGoAssets {
  logo: string;
}

interface Assets {
  shapes: ShapeAssets;
  gif: GifAsset;
  chart: ChartAssets;
  team: TeamAssets;
  fonts: GoogleFont[];
  deckdeckgo: DeckDeckGoAssets;
  navigation: ImgAsset[];
}
