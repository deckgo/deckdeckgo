export interface InitStyleColor {
  rgb: string | null;
  opacity: number | null;
}

export class ColorUtils {
  static splitColor(styleColor: string): Promise<InitStyleColor> {
    return new Promise<InitStyleColor>((resolve) => {
      if (styleColor && styleColor !== undefined) {
        const rgbs: RegExpMatchArray | null = styleColor.match(/[.?\d]+/g);

        if (rgbs && rgbs.length >= 3) {
          resolve({
            rgb: `${rgbs[0]}, ${rgbs[1]}, ${rgbs[2]}`,
            opacity: rgbs.length > 3 ? parseFloat(rgbs[3]) * 100 : 100
          });

          return;
        }
      }

      resolve({
        rgb: null,
        opacity: 100
      });
    });
  }

  static transformOpacity(colorOpacity: number): number {
    return colorOpacity === 0 ? 0 : colorOpacity / 100;
  }
}
