import React from 'react';

import {Link} from 'gatsby';

import {FormattedMessage, useIntl} from 'react-intl';

import {main, section, enterprise} from './features.module.scss';

import {Feature} from '../../core/feature/feature';

export const ListFeatures = ({lang}) => {
  const intl = useIntl();

  return (
    <section className={section}>
      <div className={main}>
        <h2>
          <FormattedMessage id="features.main.title" />
        </h2>

        <Feature titleKey="features.main.edit.title" msgKey="features.main.edit.content" />
        <Feature titleKey="features.main.showcase.title" msgKey="features.main.showcase.content" />
        <Feature titleKey="features.main.privacy.title" msgKey="features.main.privacy.content" />
        <Feature titleKey="features.main.share.title" msgKey="features.main.share.content" />
        <Feature titleKey="features.main.push.title" msgKey="features.main.push.content" />
        <Feature titleKey="features.main.privatelinks.title" msgKey="features.main.privatelinks.content" enterprise={true} />
        <Feature titleKey="features.main.pdf.title" msgKey="features.main.pdf.content" enterprise={true} />
        <Feature titleKey="features.main.polls.title" msgKey="features.main.polls.content" />
        <Feature titleKey="features.main.offline.title" msgKey="features.main.offline.content" />

        <h2>
          <FormattedMessage id="features.edit.title" />
        </h2>

        <Feature titleKey="features.edit.unbreakable.title" msgKey="features.edit.unbreakable.content" enterprise={true} />
        <Feature titleKey="features.edit.code.title" msgKey="features.edit.code.content" />
        <Feature titleKey="features.edit.embed.title" msgKey="features.edit.embed.content" />
        <Feature titleKey="features.edit.library.title" msgKey="features.edit.library.content" />
        <Feature titleKey="features.edit.youtube.title" msgKey="features.edit.youtube.content" />
        <Feature titleKey="features.edit.unsplashtenor.title" msgKey="features.edit.unsplashtenor.content" />
        <Feature titleKey="features.edit.googlefonts.title" msgKey="features.edit.googlefonts.content" />
        <Feature titleKey="features.edit.customfonts.title" msgKey="features.edit.customfonts.content" enterprise={true} />
        <Feature titleKey="features.edit.fullscreen.title" msgKey="features.edit.fullscreen.content" />
        <Feature titleKey="features.edit.transition.title" msgKey="features.edit.transition.content" />
        <Feature titleKey="features.edit.math.title" msgKey="features.edit.math.content" />
        <Feature titleKey="features.edit.charts.title" msgKey="features.edit.charts.content" />
        <Feature titleKey="features.edit.templates.title" msgKey="features.edit.templates.content" />
        <Feature titleKey="features.edit.figma.title" msgKey="features.edit.figma.content" />

        <h2>
          <FormattedMessage id="features.presenting.title" />
        </h2>

        <Feature titleKey="features.presenting.remote.title" msgKey="features.presenting.remote.content" />
        <Feature titleKey="features.presenting.notes.title" msgKey="features.presenting.notes.content" />

        <h2>
          <FormattedMessage id="features.enterprise.title" />
        </h2>

        <Feature titleKey="features.enterprise.housing.title" msgKey="features.enterprise.housing.content" enterprise={true} />
        <Feature titleKey="features.enterprise.domain.title" msgKey="features.enterprise.domain.content" enterprise={true} />
        <Feature titleKey="features.enterprise.uptodate.title" msgKey="features.enterprise.uptodate.content" enterprise={true} />
        <Feature titleKey="features.enterprise.archive.title" msgKey="features.enterprise.archive.content" enterprise={true} />
        <Feature titleKey="features.enterprise.collaborate.title" msgKey="features.enterprise.collaborate.content" enterprise={true} />
        <Feature titleKey="features.enterprise.library.title" msgKey="features.enterprise.library.content" enterprise={true} />
        <Feature titleKey="features.enterprise.support.title" msgKey="features.enterprise.support.content" enterprise={true} />

        <h2>
          <FormattedMessage id="features.developers.title" />
        </h2>

        <Feature titleKey="features.developers.kit.title" msgKey="features.developers.kit.content" />
        <Feature titleKey="features.developers.opensource.title" msgKey="features.developers.opensource.content" />

        <p className={enterprise}>
          <img loading="lazy" src="/assets/icons/ionicons/business.svg" alt={intl.formatMessage({id: 'features.enterprise.only'})} />{' '}
          <FormattedMessage
            id="features.enterprise.only.notice"
            values={{
              organizationsLink: (
                <Link to={`/${lang}/enterprise/`}>
                  <FormattedMessage id="features.enterprise.organizations.link" />
                </Link>
              )
            }}
          />
        </p>
      </div>
    </section>
  );
};
