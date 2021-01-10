import {extractRgb, extractRgba} from '@deckdeckgo/utils';

export class ContrastUtils {
  static async calculateContrastRatio(bgColor: string | undefined, color: string | undefined): Promise<number> {
    const bgColorWithDefault: string = bgColor === undefined || bgColor === '' ? `rgb(255, 255, 255)` : bgColor;
    const colorWithDefault: string = color === undefined || color === '' || color === 'initial' ? `rgb(0, 0, 0)` : color;

    // The text color may or may not be semi-transparent, but that doesn't matter
    const bgRgba: number[] | undefined = extractRgba(bgColorWithDefault);

    if (!bgRgba || bgRgba.length < 4 || bgRgba[3] >= 1) {
      return this.calculateContrastRatioOpaque(bgColorWithDefault, colorWithDefault);
    }

    return this.calculateContrastRatioAlpha(bgColorWithDefault, colorWithDefault);
  }

  private static calculateLuminance(rgb: number[]): number {
    const a = rgb.map((v) => {
      v /= 255;
      return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    return a[0] * 0.2126 + a[1] * 0.7152 + a[2] * 0.0722;
  }

  private static calculateColorContrastRatio(firstColorLum: number, secondColorLum: number): number {
    // return firstColorLum > secondColorLum ? (secondColorLum + 0.05) / (firstColorLum + 0.05) : (firstColorLum + 0.05) / (secondColorLum + 0.05);

    const l1 = firstColorLum + 0.05;
    const l2 = secondColorLum + 0.05;

    let ratio = l1 / l2;

    if (l2 > l1) {
      ratio = 1 / ratio;
    }

    return ratio;
  }

  // Source: https://github.com/LeaVerou/contrast-ratio/blob/eb7fe8f16206869f8d36d517d7eb0962830d0e81/color.js#L86
  private static async convertAlphaRgba(color: string, base: number[]): Promise<string> {
    const rgba: number[] | undefined = extractRgba(color);

    if (!rgba || rgba.length < 4) {
      return color;
    }

    const alpha: number = rgba[3];

    const rgb: number[] = [];

    for (let i = 0; i < 3; i++) {
      rgb.push(rgba[i] * alpha + base[i] * base[3] * (1 - alpha));
    }

    // Not used here
    // rgb[3] = alpha + base[3] * (1 - alpha);

    return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
  }

  private static async calculateColorContrastRatioWithBase(
    bgColor: string,
    lumColor: number,
    base: number[]
  ): Promise<{luminanceOverlay: number; ratio: number}> {
    const overlay = extractRgb(await this.convertAlphaRgba(bgColor, base));

    const lumOverlay: number = this.calculateLuminance(overlay);

    return {
      luminanceOverlay: lumOverlay,
      ratio: this.calculateColorContrastRatio(lumOverlay, lumColor),
    };
  }

  private static async calculateContrastRatioAlpha(bgColor: string, color: string): Promise<number> {
    const lumColor: number = this.calculateLuminance(extractRgb(color));

    const onBlack: {luminanceOverlay: number; ratio: number} = await this.calculateColorContrastRatioWithBase(bgColor, lumColor, [0, 0, 0, 1]);
    const onWhite: {luminanceOverlay: number; ratio: number} = await this.calculateColorContrastRatioWithBase(bgColor, lumColor, [255, 255, 255, 1]);

    const max = Math.max(onBlack.ratio, onWhite.ratio);

    let min = 1;
    if (onBlack.luminanceOverlay > lumColor) {
      min = onBlack.ratio;
    } else if (onWhite.luminanceOverlay < lumColor) {
      min = onWhite.ratio;
    }

    return (min + max) / 2;
  }

  private static async calculateContrastRatioOpaque(bgColor: string, color: string): Promise<number> {
    const bgRgb: number[] | undefined = extractRgb(bgColor);
    const colorRgb: number[] | undefined = extractRgb(color);

    if (bgColor === undefined || colorRgb === undefined) {
      // 0 being AA and AAA level friendly. We assume that if for some reason we can't extract color, we better not display a warning about it.
      return 0;
    }

    const lumBg: number = this.calculateLuminance(bgRgb);
    const lumColor: number = this.calculateLuminance(colorRgb);

    return this.calculateColorContrastRatio(lumBg, lumColor);
  }
}
