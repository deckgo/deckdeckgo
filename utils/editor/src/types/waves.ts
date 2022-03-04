export interface SvgPath {
  d: string;
}

export interface Waves {
  viewBox: string;
  width: string;
  fill: string;
  opacity: string;
  preserveAspectRatio?: string;
  wave: WavesOrientation;
  path: SvgPath;
}

export type WavesOrientation = 'upward' | 'downward';
