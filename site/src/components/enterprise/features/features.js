import React from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {main, section, enterprise} from './features.module.scss';

import {Feature} from '../feature/feature';

export const Features = () => {
  const intl = useIntl();

  return (
    <section className={section}>
      <main className={main}>
        <h2>
          <FormattedMessage id="enterprise.features.main.title" />
        </h2>

        <Feature titleKey="enterprise.features.main.edit.title" msgKey="enterprise.features.main.edit.content" />
        <Feature titleKey="enterprise.features.main.showcase.title" msgKey="enterprise.features.main.showcase.content" />
        <Feature titleKey="enterprise.features.main.privacy.title" msgKey="enterprise.features.main.privacy.content" />
        <Feature titleKey="enterprise.features.main.share.title" msgKey="enterprise.features.main.share.content" />
        <Feature titleKey="enterprise.features.main.push.title" msgKey="enterprise.features.main.push.content" />
        <Feature titleKey="enterprise.features.main.privatelinks.title" msgKey="enterprise.features.main.privatelinks.content" enterprise={true} />
        <Feature titleKey="enterprise.features.main.pdf.title" msgKey="enterprise.features.main.pdf.content" enterprise={true} />
        <Feature titleKey="enterprise.features.main.polls.title" msgKey="enterprise.features.main.polls.content" />
        <Feature titleKey="enterprise.features.main.offline.title" msgKey="enterprise.features.main.offline.content" />

        <h2>
          <FormattedMessage id="enterprise.features.edit.title" />
        </h2>

        <Feature titleKey="enterprise.features.edit.unbreakable.title" msgKey="enterprise.features.edit.unbreakable.content" enterprise={true} />
        <Feature titleKey="enterprise.features.edit.code.title" msgKey="enterprise.features.edit.code.content" />
        <Feature titleKey="enterprise.features.edit.embed.title" msgKey="enterprise.features.edit.embed.content" />
        <Feature titleKey="enterprise.features.edit.library.title" msgKey="enterprise.features.edit.library.content" />
        <Feature titleKey="enterprise.features.edit.youtube.title" msgKey="enterprise.features.edit.youtube.content" />
        <Feature titleKey="enterprise.features.edit.unsplashtenor.title" msgKey="enterprise.features.edit.unsplashtenor.content" />
        <Feature titleKey="enterprise.features.edit.googlefonts.title" msgKey="enterprise.features.edit.googlefonts.content" />
        <Feature titleKey="enterprise.features.edit.customfonts.title" msgKey="enterprise.features.edit.customfonts.content" enterprise={true} />
        <Feature titleKey="enterprise.features.edit.fullscreen.title" msgKey="enterprise.features.edit.fullscreen.content" />
        <Feature titleKey="enterprise.features.edit.transition.title" msgKey="enterprise.features.edit.transition.content" />
        <Feature titleKey="enterprise.features.edit.math.title" msgKey="enterprise.features.edit.math.content" />
        <Feature titleKey="enterprise.features.edit.charts.title" msgKey="enterprise.features.edit.charts.content" />
        <Feature titleKey="enterprise.features.edit.templates.title" msgKey="enterprise.features.edit.templates.content" />
        <Feature titleKey="enterprise.features.edit.figma.title" msgKey="enterprise.features.edit.figma.content" />

        <h2>
          <FormattedMessage id="enterprise.features.presenting.title" />
        </h2>

        <Feature titleKey="enterprise.features.presenting.remote.title" msgKey="enterprise.features.presenting.remote.content" />
        <Feature titleKey="enterprise.features.presenting.notes.title" msgKey="enterprise.features.presenting.notes.content" />

        <h2>
          <FormattedMessage id="enterprise.features.enterprise.title" />
        </h2>

        <Feature titleKey="enterprise.features.enterprise.housing.title" msgKey="enterprise.features.enterprise.housing.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.domain.title" msgKey="enterprise.features.enterprise.domain.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.uptodate.title" msgKey="enterprise.features.enterprise.uptodate.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.archive.title" msgKey="enterprise.features.enterprise.archive.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.collaborate.title" msgKey="enterprise.features.enterprise.collaborate.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.library.title" msgKey="enterprise.features.enterprise.library.content" enterprise={true} />
        <Feature titleKey="enterprise.features.enterprise.support.title" msgKey="enterprise.features.enterprise.support.content" enterprise={true} />

        <h2>
          <FormattedMessage id="enterprise.features.developers.title" />
        </h2>

        <Feature titleKey="enterprise.features.developers.kit.title" msgKey="enterprise.features.developers.kit.content" />
        <Feature titleKey="enterprise.features.developers.opensource.title" msgKey="enterprise.features.developers.opensource.content" />

        <p className={enterprise}>
          <img loading="lazy" src="/assets/icons/ionicons/business.svg" alt={intl.formatMessage({id: 'enterprise.features.enterprise.only'})} />{' '}
          <FormattedMessage id="enterprise.features.enterprise.only.notice" />
        </p>
      </main>
    </section>
  );
};
