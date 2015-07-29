package cz.auderis.deploy.descriptor.producer;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum FactoryProductionMode implements DescriptorParserSupport.NamedEnum {

	NEW_INSTANCE("NewInstance"),

	SHARED_INSTANCE("Shared")

	;

	private static FactoryProductionMode DEFAULT_MODE = NEW_INSTANCE;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	FactoryProductionMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static FactoryProductionMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(FactoryProductionMode newDefaultMode) {
		if (null == newDefaultMode) {
			throw new NullPointerException();
		}
		DEFAULT_MODE = newDefaultMode;
	}

	@Override
	public String getCanonicalName() {
		return canonicalName;
	}

	@Override
	public Set<String> getRecognizedNames() {
		return recognizedNames;
	}

	@Override
	public String toString() {
		return canonicalName;
	}

}
