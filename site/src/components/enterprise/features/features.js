import React from 'react';

import { Link } from "gatsby";

import {FormattedMessage} from 'react-intl';

import {main, section} from './features.module.scss';

import {Feature} from '../../core/feature/feature';

export const EnterpriseFeatures = ({lang}) => {
  return (
    <section className={section}>
      <main className={main}>
        <h2>
          <FormattedMessage id="enterprise.features.title" />
        </h2>

        <p>
          <FormattedMessage
            id="enterprise.features.introduction"
            values={{
              communityLink: (
                <Link to={`/${lang}/features/`} style={{textDecoration: 'underline'}}>
                  <FormattedMessage id="enterprise.community.link" />
                </Link>
              ),
            }}
          />
        </p>

        <Feature titleKey="features.main.privatelinks.title" msgKey="features.main.privatelinks.content" />
        <Feature titleKey="features.main.pdf.title" msgKey="features.main.pdf.content" />
        <Feature titleKey="features.edit.customfonts.title" msgKey="features.edit.customfonts.content" />
        <Feature titleKey="features.enterprise.housing.title" msgKey="features.enterprise.housing.content" />
        <Feature titleKey="features.enterprise.domain.title" msgKey="features.enterprise.domain.content" />
        <Feature titleKey="features.enterprise.uptodate.title" msgKey="features.enterprise.uptodate.content" />
        <Feature titleKey="features.enterprise.archive.title" msgKey="features.enterprise.archive.content" />
        <Feature titleKey="features.enterprise.collaborate.title" msgKey="features.enterprise.collaborate.content" />
        <Feature titleKey="features.enterprise.library.title" msgKey="features.enterprise.library.content" />
        <Feature titleKey="features.enterprise.support.title" msgKey="features.enterprise.support.content" />

      </main>
    </section>
  );
};
