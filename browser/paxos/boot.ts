import {bootstrap}    from 'angular2/platform/browser';
import {provide}    from 'angular2/core';
import {AppComponent} from './components/app/app';
import {APP_BASE_HREF, ROUTER_PROVIDERS, LocationStrategy, PathLocationStrategy} from 'angular2/router';
import {HTTP_PROVIDERS} from 'angular2/http';

bootstrap(AppComponent, [
  ROUTER_PROVIDERS,
  provide(APP_BASE_HREF, {useValue: '/'}),
  HTTP_PROVIDERS
]);
