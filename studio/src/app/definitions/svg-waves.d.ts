interface SvgPath {
  d: string;
}

interface SvgWaves {
  viewBox: string;
  width: string;
  fill: string;
  opacity: string;
  preserveAspectRatio?: string;
  style: Partial<Record<keyof CSSStyleDeclaration, string>>;
  path: SvgPath;
}

type SvgWavesOrientation = 'upward' | 'downward';
