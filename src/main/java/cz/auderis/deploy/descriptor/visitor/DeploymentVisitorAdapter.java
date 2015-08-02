/*
 * Copyright 2015 Boleslav Bobcik - Auderis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package cz.auderis.deploy.descriptor.visitor;

import cz.auderis.deploy.descriptor.Deployment;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.deploy.descriptor.producer.FactoryDefinition;

public class DeploymentVisitorAdapter implements DeploymentVisitor {

	@Override
	public void visitDeployment(Deployment deployment) {
		// No operation
	}

	@Override
	public void visitNormalBean(NormalBean normalBean) {
		// No operation
	}

	@Override
	public void visitListBean(StandaloneListBean listBean) {
		// No operation
	}

	@Override
	public void visitSetBean(StandaloneSetBean setBean) {
		// No operation
	}

	@Override
	public void visitMapBean(StandaloneMapBean mapBean) {
		// No operation
	}

	@Override
	public void visitBundleBean(ExternalBundleBean bundleBean) {
		// No operation
	}

	@Override
	public void visitFactoryDefinition(FactoryDefinition factory) {
		// No operation
	}
}
