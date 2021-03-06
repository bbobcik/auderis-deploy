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

package cz.auderis.deploy.processor.validator;

import java.io.Serializable;

public class SyntaxCheckMessage implements Serializable {
	private static final long serialVersionUID = 20151030L;

	private final String message;

	public SyntaxCheckMessage(String message) {
		if (null == message) {
			throw new NullPointerException();
		}
		this.message = message;
	}

	public String getMessage() {
		return message;
	}

}
