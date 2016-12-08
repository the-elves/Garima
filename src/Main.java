import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;

public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		HashMap <String, String> signalExecs, effectExecs, sitsMappings;
		String[] signals;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream("/home/ajinkya/College/PLDI"
					+ "/Garima/src/config.txt")));
			String signalsstr = br.readLine();
			signalExecs = new HashMap<String,String>();
			effectExecs = new HashMap<String,String>();
			sitsMappings = new HashMap<String,String>();
			
			signalsstr = signalsstr.substring(1, signalsstr.length()-1);	
			signals = signalsstr.split(",");
			for(String s:signals){
				String split[];
				s = s.substring(1, s.length()-1);
				split = s.split(" ");
				signalExecs.put(split[0], split[1]);
			}
			System.out.println(signalExecs.toString());
			
			signalsstr = br.readLine();
			
			effectExecs = new HashMap<String,String>();
			
			signalsstr = signalsstr.substring(1, signalsstr.length()-1);	
			signals = signalsstr.split(",");
			for(String s:signals){
				String split[];
				s = s.substring(1, s.length()-1);
				split = s.split(" ");
				effectExecs.put(split[0], split[1]);
			}
			System.out.println(effectExecs.toString());
			
			signalsstr = br.readLine();
			
			sitsMappings = new HashMap<String,String>();
			
			signalsstr = signalsstr.substring(1, signalsstr.length()-1);	
			signals = signalsstr.split(",");
			for(String s:signals){
				String split[];
				s = s.substring(1, s.length()-1);
				split = s.split(" ");
				sitsMappings.put(split[1], split[0]);
			}
			System.out.println(sitsMappings.toString());
			
	        try
	        {  
	        	Runtime rt = Runtime.getRuntime();
	        	while(true){
	        		for(String s:signalExecs.keySet()){
	        			String base = "/home/ajinkya/College/PLDI/Garima/src/";
			            Process proc = rt.exec(base+signalExecs.get(s));
			            int exitVal = proc.waitFor();
			            if(exitVal!=0 && sitsMappings.containsKey(s)){
			            	Runtime rt1 = Runtime.getRuntime();
			            	Process innerProc = rt1.exec(base + sitsMappings.get(s));
					 InputStream stderr = proc.getInputStream();
			                 InputStreamReader isr = new InputStreamReader(stderr);
			                 BufferedReader br1 = new BufferedReader(isr);
			                 String line = null;
					 System.out.println("<Stdout>");
			                 while ( (line = br1.readLine()) != null)
			     	            System.out.println(line);
			                 System.out.println("</stdout>");
			            	
			            }
			            InputStream stderr = proc.getInputStream();
			            InputStreamReader isr = new InputStreamReader(stderr);
			            BufferedReader br1 = new BufferedReader(isr);
			            
	        		}
	        		Thread.sleep(1000);
		            
	        	}
 	        } catch (Throwable t)
	          {
	            t.printStackTrace();
	          }
			   
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
