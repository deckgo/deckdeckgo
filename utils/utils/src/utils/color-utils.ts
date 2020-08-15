// https://stackoverflow.com/a/5624139/5404186
export async function hexToRgb(hex: string): Promise<string | undefined> {
  if (!hex || hex === undefined || hex === '') {
    return undefined;
  }

  const result: RegExpExecArray | null = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);

  return result ? `${parseInt(result[1], 16)}, ${parseInt(result[2], 16)}, ${parseInt(result[3], 16)}` : undefined;
}

// https://stackoverflow.com/a/42429333/5404186
export async function rgbToHex(rgb: string | undefined): Promise<string | undefined> {
  if (!rgb) {
    return undefined;
  }

  const toHex = (rgb: number[] | undefined): string | undefined => {
    if (!rgb) {
      return undefined;
    }

    return `#${rgb.map((v) => v.toString(16).padStart(2, '0')).join('')}`;
  };

  return toHex(extractRgb(rgb));
}

export function extractRgb(rgb: string): number[] | undefined {
  const match: RegExpMatchArray | null = rgb.match(/([.\d]+),\s*([.\d]+),\s*([.\d]+)/);

  if (!match) {
    return undefined;
  }

  return match.splice(1, 3).map((v) => Number(v));
}

export function extractRgba(rgb: string): number[] | undefined {
  const match: RegExpMatchArray | null = rgb.match(/([.\d]+),\s*([.\d]+),\s*([.\d]+),\s*([.\d]+)/);

  if (!match) {
    return undefined;
  }

  return match.splice(1, 4).map((v) => Number(v));
}
