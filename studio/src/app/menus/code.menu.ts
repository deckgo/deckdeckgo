import {popoverController} from '@ionic/core';

import {StyloMenu} from '@papyrs/stylo';

import {SelectedTarget} from '../types/editor/selected-target';

import {SelectedElementUtils} from '../utils/editor/selected-element.utils';

export const codeMenu: StyloMenu = {
  match: ({paragraph}: {paragraph: HTMLElement}) => paragraph?.nodeName.toLowerCase() === 'deckgo-highlight-code',
  actions: [
    {
      text: 'edit_code',
      icon: `<svg xmlns="http://www.w3.org/2000/svg" width='20' height='20' viewBox="0 0 512 512">
                <path d="M384 224v184a40 40 0 01-40 40H104a40 40 0 01-40-40V168a40 40 0 0140-40h167.48" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="32"/>
                <path d="M459.94 53.25a16.06 16.06 0 00-23.22-.56L424.35 65a8 8 0 000 11.31l11.34 11.32a8 8 0 0011.34 0l12.06-12c6.1-6.09 6.67-16.01.85-22.38zM399.34 90L218.82 270.2a9 9 0 00-2.31 3.93L208.16 299a3.91 3.91 0 004.86 4.86l24.85-8.35a9 9 0 003.93-2.31L422 112.66a9 9 0 000-12.66l-9.95-10a9 9 0 00-12.71 0z"/>
             </svg>`,
      action: async ({paragraph}: {paragraph: HTMLElement}) => {
        const editCode: CustomEvent<void> = new CustomEvent<void>('editCode', {
          bubbles: true
        });

        paragraph.dispatchEvent(editCode);
      }
    },
    {
      text: 'style',
      icon: `<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' viewBox='0 0 512 512'>
              <path d='M452.37,59.63h0a40.49,40.49,0,0,0-57.26,0L184,294.74c23.08,4.7,46.12,27.29,49.26,49.26L452.37,116.89A40.49,40.49,0,0,0,452.37,59.63Z' style='fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/>
              <path d='M138,336c-29.88,0-54,24.5-54,54.86,0,23.95-20.88,36.57-36,36.57C64.56,449.74,92.82,464,120,464c39.78,0,72-32.73,72-73.14C192,360.5,167.88,336,138,336Z' style='fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/>
            </svg>`,
      action: async ({paragraph}: {paragraph: HTMLElement}) => {
        const popover: HTMLIonPopoverElement = await popoverController.create({
          component: 'app-element-style',
          componentProps: {
            selectedTarget: {
              target: paragraph,
              ...(SelectedElementUtils.initDescription(paragraph) as SelectedTarget)
            }
          },
          mode: 'ios',
          showBackdrop: false,
          cssClass: 'popover-menu'
        });

        await popover.present();
      }
    },
    {
      text: 'options',
      icon: `<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' viewBox='0 0 512 512'>
            <path d='M262.29,192.31a64,64,0,1,0,57.4,57.4A64.13,64.13,0,0,0,262.29,192.31ZM416.39,256a154.34,154.34,0,0,1-1.53,20.79l45.21,35.46A10.81,10.81,0,0,1,462.52,326l-42.77,74a10.81,10.81,0,0,1-13.14,4.59l-44.9-18.08a16.11,16.11,0,0,0-15.17,1.75A164.48,164.48,0,0,1,325,400.8a15.94,15.94,0,0,0-8.82,12.14l-6.73,47.89A11.08,11.08,0,0,1,298.77,470H213.23a11.11,11.11,0,0,1-10.69-8.87l-6.72-47.82a16.07,16.07,0,0,0-9-12.22,155.3,155.3,0,0,1-21.46-12.57,16,16,0,0,0-15.11-1.71l-44.89,18.07a10.81,10.81,0,0,1-13.14-4.58l-42.77-74a10.8,10.8,0,0,1,2.45-13.75l38.21-30a16.05,16.05,0,0,0,6-14.08c-.36-4.17-.58-8.33-.58-12.5s.21-8.27.58-12.35a16,16,0,0,0-6.07-13.94l-38.19-30A10.81,10.81,0,0,1,49.48,186l42.77-74a10.81,10.81,0,0,1,13.14-4.59l44.9,18.08a16.11,16.11,0,0,0,15.17-1.75A164.48,164.48,0,0,1,187,111.2a15.94,15.94,0,0,0,8.82-12.14l6.73-47.89A11.08,11.08,0,0,1,213.23,42h85.54a11.11,11.11,0,0,1,10.69,8.87l6.72,47.82a16.07,16.07,0,0,0,9,12.22,155.3,155.3,0,0,1,21.46,12.57,16,16,0,0,0,15.11,1.71l44.89-18.07a10.81,10.81,0,0,1,13.14,4.58l42.77,74a10.8,10.8,0,0,1-2.45,13.75l-38.21,30a16.05,16.05,0,0,0-6.05,14.08C416.17,247.67,416.39,251.83,416.39,256Z' style='fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/>
          </svg>`,
      action: async ({paragraph}: {paragraph: HTMLElement}) => {
        const popover: HTMLIonPopoverElement = await popoverController.create({
          component: 'app-code',
          componentProps: {
            selectedTarget: paragraph
          },
          mode: 'ios',
          showBackdrop: false,
          cssClass: 'popover-menu'
        });

        await popover.present();
      }
    }
  ]
};
