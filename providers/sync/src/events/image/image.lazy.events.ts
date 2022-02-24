export class ImageLazyEvents {
  private treeObserver: MutationObserver | undefined;

  init(containerRef: HTMLElement) {
    this.treeObserver = new MutationObserver(this.onTreeMutation);
    this.treeObserver.observe(containerRef, {childList: true, subtree: true});
  }

  destroy() {
    this.treeObserver?.disconnect();
  }

  private onTreeMutation = async (mutations: MutationRecord[]) => {
    const addedNodes: Node[] = mutations.reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

    const imgNode: Node | undefined = addedNodes.find((node: Node) => node.nodeName?.toLowerCase() === 'deckgo-lazy-img');

    if (!imgNode) {
      return;
    }

    const element: HTMLDeckgoLazyImgElement = imgNode as HTMLDeckgoLazyImgElement;
    element.customLoader = true;
    await element.lazyLoad();
  };
}
