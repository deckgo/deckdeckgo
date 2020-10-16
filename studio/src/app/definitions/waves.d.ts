interface SvgPath {
  d: string;
}

interface Waves {
  viewBox: string;
  width: string;
  fill: string;
  opacity: string;
  preserveAspectRatio?: string;
  style: Partial<Record<keyof CSSStyleDeclaration, string>>;
  path: SvgPath;
}

type WavesOrientation = 'upward' | 'downward';
