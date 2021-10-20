import {isIOS} from '@deckdeckgo/utils';

export const containerSize = ({embedded, container}: {embedded: boolean; container: HTMLElement}): {width: number; height: number} => {
  if (!embedded) {
    return screenSize();
  }

  if (!container || !container.offsetParent || container.offsetParent.clientWidth <= 0) {
    return screenSize();
  }

  return {
    width: container.offsetParent.clientWidth,
    height: container.offsetParent.clientHeight
  };
};

const screenSize = (): {width: number; height: number} => {
  if (isIOS()) {
    return {
      width: screen.width > window.innerWidth ? screen.width : window.innerWidth,
      height: screen.height > window.innerHeight ? screen.height : window.innerHeight
    };
  }

  return {
    width: window.innerWidth,
    height: window.innerHeight
  };
};
