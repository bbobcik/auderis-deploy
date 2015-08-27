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

package cz.auderis.deploy.descriptor;

import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import java.util.ArrayList;
import java.util.List;

public class DeploymentCollection implements VisitableNode {

	final List<Deployment> deployments;

	public DeploymentCollection() {
		this.deployments = new ArrayList<Deployment>(2);
	}

	public List<Deployment> getDeployments() {
		return deployments;
	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		for (final Deployment deployment : deployments) {
			deployment.accept(visitor, context);
		}
	}

}
