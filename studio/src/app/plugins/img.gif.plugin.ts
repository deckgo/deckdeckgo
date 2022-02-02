import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@papyrs/stylo';

import {openPluginModal} from '../utils/editor/plugin.utils';

export const imgGif: StyloPlugin = {
  text: 'gifs',
  icon: `<svg width="32" height="32" viewBox="0 0 512 512" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;">
        <path d="M464,128C464,101.508 442.492,80 416,80L96,80C69.508,80 48,101.508 48,128L48,384C48,410.492 69.508,432 96,432L416,432C442.492,432 464,410.492 464,384L464,128Z" style="fill:none;stroke:currentColor;stroke-width:32px;"/>
        <g transform="matrix(1,0,0,0.843266,1.54944,39.8473)">
            <path d="M160.948,247.902L217.589,247.902L217.589,335.684C208.409,339.235 199.766,341.725 191.661,343.153C183.555,344.582 175.271,345.296 166.807,345.296C145.258,345.296 128.803,337.788 117.442,322.771C106.082,307.755 100.401,286.196 100.401,258.093C100.401,230.763 106.993,209.454 120.177,194.168C133.36,178.881 151.638,171.238 175.011,171.238C189.659,171.238 203.787,174.712 217.393,181.66L207.335,210.381C196.918,204.204 186.078,201.116 174.815,201.116C161.729,201.116 151.248,206.327 143.37,216.75C135.492,227.173 131.554,241.185 131.554,258.788C131.554,277.163 134.727,291.195 141.075,300.884C147.423,310.573 156.651,315.418 168.761,315.418C175.076,315.418 181.488,314.646 187.999,313.101L187.999,277.78L160.948,277.78L160.948,247.902Z" style="fill-rule:nonzero;"/>
            <rect x="251.573" y="173.67" width="30.273" height="169.31" style="fill-rule:nonzero;"/>
            <path d="M347.569,342.98L317.784,342.98L317.784,173.67L399.62,173.67L399.62,203.085L347.569,203.085L347.569,246.744L396.007,246.744L396.007,276.043L347.569,276.043L347.569,342.98Z" style="fill-rule:nonzero;"/>
        </g>
    </svg>
  `,
  createParagraphs: (pluginParams: StyloPluginCreateParagraphsParams) => openPluginModal({pluginParams, componentTag: 'app-gif'})
};
