export class RevealUtils {
  static findChildren(el: HTMLElement): Promise<Node[]> {
    return new Promise<Node[]>((resolve) => {
      const elements: Node[] = Array.from(el.childNodes).filter((node: Node) => {
        return node && node.nodeType !== node.TEXT_NODE;
      });

      resolve(elements);
    });
  }
}
