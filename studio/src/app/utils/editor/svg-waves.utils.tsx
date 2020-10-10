export class SvgWavesUtils {
  static generateCoordinates(nodes: number): [string, string][] {
    const unit = 500 / nodes;
    const arr = new Array(nodes).fill(0);
    arr.forEach((_a, i) => {
      const randomSign = Math.random() > 0.5 ? 1 : -1;
      const before = arr[i - 1] ? arr[i - 1][1] : 50;
      const add = Math.max(Math.min(Math.random() * before, 75), 25);
      arr[i] = [(unit * (i + 1)).toFixed(2), Math.max(Math.min(Number(before) + add * randomSign, 115), 35).toFixed(2)];
    });
    return arr;
  }

  private static getWavesPath(nodes: number, coordinates?: [string, string][]): string {
    let coords = coordinates;
    if (!coords) {
      coords = this.generateCoordinates(nodes);
    }
    const startY = (50 + (Math.random() * 500) / nodes).toFixed(2);
    const smoothPoints = new Array(nodes / 2).fill(0).map((_c, i) => `S${coords.slice(i * 2, 2 + i * 2).join(' ')}`);
    const curves = smoothPoints.join(' ');
    return `M0.00,${startY} ${curves}`;
  }

  private static getClosingPath(orientation: SvgWavesOrientation): string {
    return orientation === 'upward' ? 'L500.00,150.00 L0.00,150.00' : 'L500.00,0.00 L0.00,0.00';
  }

  static getFullPath(nodes: number, orientation: SvgWavesOrientation, coordinates?: [string, string][]): string {
    return `${this.getWavesPath(nodes, coordinates)} ${this.getClosingPath(orientation)} Z`;
  }
}
