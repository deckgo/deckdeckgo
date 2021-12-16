import {videoStart, videoPause} from './play-pause';
import {previousSlide, nextSlide, toggleFullScreen} from './slider';
import {openRemote} from '../modals/remote';
import {presentSlidePicker} from '../menu/chapters';
import {openMenu} from '../menu/menu';

export const initButtons = () => {
  document.addEventListener('DOMContentLoaded', async () => {
    attachClickListener('play', videoStart);
    attachClickListener('pause', videoPause);
    attachClickListener('previous', previousSlide);
    attachClickListener('next', nextSlide);
    attachClickListener('slidePicker', presentSlidePicker);
    attachClickListener('fullScreen', toggleFullScreen);
    attachClickListener('remote', openRemote);
    attachClickListener('menu', openMenu);
  });
};

function attachClickListener(selectorId: string, action: ($event: UIEvent) => Promise<void>) {
  const element: HTMLElement | null = document.getElementById(selectorId);

  if (!element) {
    return;
  }

  element.addEventListener('click', action);
}
