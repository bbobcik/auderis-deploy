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

package cz.auderis.deploy.descriptor.bean;

public enum BeanLifecycleStage {

	NOT_INITIALIZED(true, false),

	EAGER_CONFLICT_FAILURE(false, true),

	LAZY_CONFLICT_FAILURE(false, true),

	PARSED(true, false),

	DEPENDENCY_FAILURE(false, true),

	DEPENDENCIES_RESOLVED(true, false),

	INJECTION_FAILURE(false, true),

	INJECTION_VALIDATED(true, false),

	CONSTRUCTED(true, true),

	;

	public boolean isFailState() {
		return failure;
	}

	public boolean isTerminalState() {
		return terminalState;
	}

	private final boolean failure;
	private final boolean terminalState;

	BeanLifecycleStage(boolean okState, boolean terminalState) {
		this.failure = !okState;
		this.terminalState = terminalState;
	}

}
