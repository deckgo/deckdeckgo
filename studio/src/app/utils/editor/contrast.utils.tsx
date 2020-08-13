export interface ParentsColors {
  slideBgColor: string | undefined;
  slideColor: string | undefined;
  deckBgColor: string | undefined;
  deckColor: string | undefined;
}

export class ContrastUtils {
  static async calculateContrastRatio(element: HTMLElement, parentsColors: ParentsColors): Promise<number> {
    const style: CSSStyleDeclaration = window.getComputedStyle(element);

    // TODO: how to handle alpha? both in custom color as in default backgroundColor rba(0,0,0,0)

    const bgColor: string =
      element.style.background !== '' && element.style.background !== 'initial'
        ? style.backgroundColor
        : parentsColors.slideBgColor !== ''
        ? parentsColors.slideBgColor
        : parentsColors.deckBgColor !== ''
        ? parentsColors.deckBgColor
        : style.backgroundColor;
    const color: string =
      element.style.color !== '' && element.style.color !== 'initial'
        ? style.color
        : parentsColors.slideColor !== ''
        ? parentsColors.slideColor
        : parentsColors.deckColor !== ''
        ? parentsColors.deckColor
        : style.color;

    // TODO utils
    const extractRgb = (rgb: string): number[] | undefined => {
      const match: RegExpMatchArray | null = rgb.match(/(\d+),\s*(\d+),\s*(\d+)/);

      if (!match) {
        return undefined;
      }

      return match.splice(1, 3).map((v) => Number(v));
    };

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

  private static calculateLuminance(rgb: number[]): number {
    const a = rgb.map((v) => {
      v /= 255;
      return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    return a[0] * 0.2126 + a[1] * 0.7152 + a[2] * 0.0722;
  }

  private static calculateColorContrastRatio(firstColor: number, secondColor: number): number {
    return firstColor > secondColor ? (secondColor + 0.05) / (firstColor + 0.05) : (firstColor + 0.05) / (secondColor + 0.05);
  }
}
