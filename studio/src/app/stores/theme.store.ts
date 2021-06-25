import {createStore} from '@stencil/store';

interface DarkThemeStore {
  darkTheme: boolean | undefined;
}

const {state, onChange} = createStore({
  darkTheme: undefined
} as DarkThemeStore);

onChange('darkTheme', (dark: boolean | undefined) => {
  const domBodyClassList: DOMTokenList = document.body.classList;
  dark ? domBodyClassList.add('dark') : domBodyClassList.remove('dark');
});

export default {state};
