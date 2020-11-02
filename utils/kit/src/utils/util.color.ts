// Ionic Color Generator: https://github.com/ionic-team/ionic-docs/blob/master/src/components/color-gen/color.ts

interface RGB {
  b: number;
  g: number;
  r: number;
}

function rgbToYIQ({r, g, b}: RGB): number {
  return (r * 299 + g * 587 + b * 114) / 1000;
}

// https://stackoverflow.com/a/5624139/5404186
function hexToRgb(hex: string): RGB | undefined {
  if (!hex || hex === undefined || hex === '') {
    return undefined;
  }

  const result: RegExpExecArray | null = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);

  return result
    ? {
        r: parseInt(result[1], 16),
        g: parseInt(result[2], 16),
        b: parseInt(result[3], 16),
      }
    : undefined;
}

export function contrast(colorRgb: string | undefined, threshold: number = 128, invert: boolean = false): string {
  const rgb: number[] | undefined = extractRgb(colorRgb);

  if (rgb === undefined || rgb.length < 3) {
    return invert ? '#fff' : '#000';
  }

  return rgbToYIQ({r: rgb[0], g: rgb[1], b: rgb[2]}) >= threshold ? (invert ? '#fff' : '#000') : invert ? '#000' : '#fff';
}

function extractRgb(rgb: string | undefined): number[] | undefined {
  if (rgb === undefined) {
    return undefined;
  }

  const match: RegExpMatchArray | null = rgb.match(/([.\d]+),\s*([.\d]+),\s*([.\d]+)/);

  if (!match) {
    return undefined;
  }

  return match.splice(1, 3).map((v) => Number(v));
}
