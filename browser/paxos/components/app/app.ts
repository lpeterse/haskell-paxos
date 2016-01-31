import { Component, ViewEncapsulation } from 'angular2/core';
import { RouteConfig, ROUTER_DIRECTIVES } from 'angular2/router';
import { componentProxyFactory } from '../../factories/component_proxy';
import { RouterActive } from '../../directives/routerActive';

@Component({
  selector: 'app',
  templateUrl: './paxos/components/app/app.html',
  styleUrls: ['./paxos/components/app/app.css'],
  encapsulation: ViewEncapsulation.None,
  directives: [ROUTER_DIRECTIVES, RouterActive]
})
@RouteConfig([
  {
    path: '/about',
    component: componentProxyFactory({
      path: './paxos/components/about/about',
      provide: m => m.AboutComponent
    }),
    as: 'About'
  }, {
    path: '/nodes',
    useAsDefault: true,
    component: componentProxyFactory({
      path: './paxos/components/nodes/nodes',
      provide: m => m.NodeComponent
    }),
    as: 'Nodes'
  }
])
export class AppComponent {}
