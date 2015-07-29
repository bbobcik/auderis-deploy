package cz.auderis.deploy.descriptor;

public class DescriptorParsingException extends RuntimeException {
	private static final long serialVersionUID = -6521169869168990389L;

	public DescriptorParsingException() {
		super();
	}

	public DescriptorParsingException(String message) {
		super(message);
	}

	public DescriptorParsingException(String message, Throwable cause) {
		super(message, cause);
	}

}
