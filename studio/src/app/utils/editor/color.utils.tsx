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

  static rgbaToHex(rgba: string): string {
    const inParts = rgba.substring(rgba.indexOf("(")).split(",");
    const r = parseInt((inParts[0].substring(1).trim()), 10);
    const g = parseInt((inParts[1].trim()), 10);
    const b = parseInt((inParts[2].trim()), 10);
    const a = parseFloat((inParts[3].substring(0, inParts[3].length - 1)));
    const outParts = [
      r.toString(16),
      g.toString(16),
      b.toString(16),
      Math.round(a * 255).toString(16).substring(0, 2)
    ];

    // Pad single-digit output values
    outParts.forEach(function (part, i) {
      if (part.length === 1) {
        outParts[i] = '0' + part;
      }
    })

    return ('#' + outParts.join(''));
  }

  static hexToRGBA(hex, alpha) {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return "rgba(" + r + ", " + g + ", " + b + ", " + alpha + ")";
  }

  static transformOpacity(colorOpacity: number): number {
    return colorOpacity === 0 ? 0 : colorOpacity / 100;
  }
}