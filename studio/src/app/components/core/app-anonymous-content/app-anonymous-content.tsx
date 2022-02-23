import {FunctionalComponent, h} from '@stencil/core';
import i18n from '../../../stores/i18n.store';
import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';

interface AppGuardedContentProps {
  title: string;
  text: string;
}

export const AppAnonymousContent: FunctionalComponent<AppGuardedContentProps> = ({title, text}) => {
  const renderNotLoggedInContent = () => {
    return renderI18n(text, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  };

  return (
    <main class="ion-padding fit">
      <h1>{title}</h1>

      {renderNotLoggedInContent()}
    </main>
  );
};
