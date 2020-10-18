interface SvgPath {
  d: string;
}

interface Waves {
  viewBox: string;
  width: string;
  fill: string;
  opacity: string;
  preserveAspectRatio?: string;
  wave: WavesOrientation;
  path: SvgPath;
}

type WavesOrientation = 'upward' | 'downward';
