class Hello
{
    int g;
    
    void main()
    {
        blob();
		int x;
		x = 12;
    }
	
	void blob()
	{
		
	}
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;   
    }
	//test
	
	int onzin()
	{
		return 3;
	}

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
   }
}
