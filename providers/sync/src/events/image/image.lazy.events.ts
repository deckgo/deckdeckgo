import {isElementNode} from '@deckdeckgo/editor';

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

    const images: HTMLDeckgoLazyImgElement[] = addedNodes.reduce((acc: HTMLDeckgoLazyImgElement[], node: Node) => {
      if (!isElementNode(node)) {
        return acc;
      }

      if (node.nodeName?.toLowerCase() === 'deckgo-lazy-img') {
        return [...acc, node as HTMLDeckgoLazyImgElement];
      }

      const img: NodeListOf<HTMLDeckgoLazyImgElement> = (node as HTMLElement).querySelectorAll('deckgo-lazy-img');
      return [...acc, ...Array.from(img)];
    }, []);

    for (const element of images) {
      element.customLoader = true;
      await element.lazyLoad();
    }
  };
}
