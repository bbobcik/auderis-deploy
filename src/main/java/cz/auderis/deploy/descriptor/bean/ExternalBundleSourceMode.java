package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum ExternalBundleSourceMode implements DescriptorParserSupport.NamedEnum {

	FIRST_OR_FAIL("First", "Single"),

	LENIENT_UNION("Union", "Join", "All"),

	;

	private static ExternalBundleSourceMode DEFAULT_MODE = FIRST_OR_FAIL;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	ExternalBundleSourceMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static ExternalBundleSourceMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(ExternalBundleSourceMode newDefaultMode) {
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
